#!/usr/bin/env node

var process = require('process');
var assert = require('assert');
var utf8 = require('utf8');
var fs = require('fs');

var icalendar = require('icalendar');

var stdin = process.stdin;
var stdout = process.stdout;

assert(process.argv.hasOwnProperty('2'));
var chosen_courses = JSON.parse(process.argv[2]);

var courses_data = JSON.parse(fs.readFileSync('courses_data.json').toString());

function print_json(obj) {
	stdout.write(JSON.stringify(obj, null, '') + '\n');
}

function print_event(event) {
	stdout.write(
		(event.all_day ? event.day : event.from + ' -> ' + event.to) + ': '
		+ (event.course_name || '?')
		+ ' in ' + (event.room || '?')
		+ ' with ' + (event.teachers || '?')
		+ (event.comments ? (' (' + event.comments.replace(/\n/g, ' ') + ')') : '')
		+ ' [' + (event.source || '?') + ']\n'
	);
}

function assert_is_day(day) {
	assert(day.type === 'day');
	assert(day.hasOwnProperty('date'));
	assert(day.hasOwnProperty('month'));
}

function assert_is_time(time) {
	assert(time.type === 'time');
	assert(time.hasOwnProperty('hours'));
	assert(time.hasOwnProperty('minutes'));
}

function assert_is_period(period) {
	assert(period.type === 'period');
	assert(period.hasOwnProperty('from_time'));
	assert(period.hasOwnProperty('to_time'));
}

function assert_is_course(course) {
	assert(course.type === 'course');
	assert(course.hasOwnProperty('number'));
	assert(course.hasOwnProperty('name'));
}

function cr(n) {
	if (n < 10) {
		return 'CR0' + n;
	} else {
		return 'CR' + n;
	}
}

function print_ics_event(event) {
	
}

var text = "";
stdin.setEncoding('utf8');
stdin.on('data', function(chunk) { text += chunk });
stdin.on('end', function() {
	var events = JSON.parse(text);
	
	var new_events = events.map(function (event) {
		var new_event = {};

		assert(event.hasOwnProperty('source'));
		new_event.source = event.source;

		assert(event.hasOwnProperty('day'));
		var day = event.day;
		assert_is_day(day);
	
		if (event.hasOwnProperty('period')) {
			var period = event.period;
			assert_is_period(period);
			var from_time = period.from_time;
			assert_is_time(from_time);
			var to_time = period.to_time;
			assert_is_time(to_time);
			new_event.all_day = false;
			new_event.from = new Date(2016, day.month - 1, day.date, from_time.hours, from_time.minutes);
			new_event.to = new Date(2016, day.month - 1, day.date, to_time.hours, to_time.minutes);
		} else {
			new_event.all_day = true;
			new_event.day = new Date(Date.UTC(2016, day.month - 1, day.date));
		}

		if (event.hasOwnProperty('course')) {
			var course = event.course;
			assert_is_course(course);
			new_event.course_number = course.number;
			new_event.course_name = course.name;
		}

		if (event.hasOwnProperty('room')) {
			new_event.room = event.room;
		}

		if (event.hasOwnProperty('teachers')) {
			new_event.teachers = event.teachers;
		}

		if (event.hasOwnProperty('comments')) {
			new_event.comments = event.comments;
		}

		return new_event;
	});
	
	var cr00_events = (function () {
		var lundis = ['24/10', '07/11', '14/11', '21/11', '28/11'];
		var jeudis = ['22/09', '29/09', '06/10', '13/10', '20/10', '03/11', '10/11', '17/11', '24/11', '01/12', '08/12', '15/12', '22/12'];
		return lundis.concat(jeudis).map(function(date_string) {
			var date_parts = date_string.split('/');
			var day = parseInt(date_parts[0], 10);
			var month = parseInt(date_parts[1], 10);
			return {
				all_day: false,
				from: new Date(2016, month - 1, day, 9),
				to: new Date(2016, month - 1, day, 12),
				course_number: 0,
				room: "Salle 125, bâtiment Braconnier, la Doua (par défaut)"
			};
		});
	})();
	
	var chosen_events = new_events.concat(cr00_events).filter(function(event) {
		return !event.hasOwnProperty('course_number') || chosen_courses[cr(event.course_number)];
	});

	var ics_calendar = new icalendar.iCalendar();
	ics_calendar.setProperty('PRODID', "M2 IF");
	chosen_events.forEach(function(event) {
		var ics_event = ics_calendar.addComponent('VEVENT');

		if (event.all_day) {
			var from = new Date(+event.day);
			from.date_only = true;
			var to = new Date(+event.day + (24 * 60 * 60 * 1000));
			to.date_only = true;
			ics_event.setDate(from, to);
		} else {
			ics_event.setDate(event.from, event.to);
		}
		
		if (event.hasOwnProperty('source')) {
			ics_event.addProperty('URL', event.source);
		}
		
		var summary = "";
		if (event.hasOwnProperty('course_name')) {
			summary = event.course_name;
		} else if (event.hasOwnProperty('comments')) {
			summary = event.comments;
		}
		if (event.hasOwnProperty('course_number')) {
			summary += ': ' + courses_data[cr(event.course_number)].name;
		}
		ics_event.setSummary(summary);
		
		if (event.hasOwnProperty('room')) {
			ics_event.setLocation(event.room);
		}
		
		if (event.hasOwnProperty('teachers')) {
			ics_event.setProperty('CONTACT', event.teachers);
		} else if (event.hasOwnProperty('course_number')) {
			ics_event.setProperty('CONTACT', courses_data[cr(event.course_number)].teachers);
		}
		
		var description = "";
		if (event.hasOwnProperty('comments')) {
			description += 'Unrecognized data:\n';
			description += event.comments + '\n\n';
		}
		if (event.hasOwnProperty('teachers')) {
			description += 'Teachers: ' + event.teachers + '\n\n';
		}
		if (event.hasOwnProperty('source')) {
			description += 'Source: ' + event.source + '\n\n';
		}
		if (event.hasOwnProperty('course_number')) {
			description += 'Inferred from course number:\n';
			var crx = cr(event.course_number);
			description += crx + ': ' + courses_data[crx].name + '\n';
			description += courses_data[crx].teachers + '\n\n';
		}
		ics_event.setDescription(description);
	});
	
	stdout.write(ics_calendar.toString());
});

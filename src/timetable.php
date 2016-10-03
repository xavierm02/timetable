<?php
	
	if (
		!file_exists('events.json')
		|| time () - filemtime ('events.json') >= 10 * 60
		|| (file_exists('pdftoics.errors') && file_get_contents('pdftoics.errors') === '')
	) {
		shell_exec('(ocamlbuild -quiet -use-ocamlfind pdftoics.native -- | konwert isolatin1-utf8) >events.json 2>pdftoics.errors');
	}
	
	$courses_data = json_decode(file_get_contents('courses_data.json'), true);
	$data = array();
	foreach ($courses_data as $cr => $course_data) {
		$data[$cr] = array_key_exists($cr, $_GET) ? $_GET[$cr] === 'on' : false;
	}
	$arg = addslashes(json_encode($data));
	$ics = shell_exec("cat events.json | node generate_calendar.js \"$arg\"");
	
	if (array_key_exists('action', $_GET) && $_GET['action'] === 'download') {
		header('Content-type: text/calendar; charset=utf-8');
		header('Content-Disposition: attachment; filename=M2IF.ics');
	}
	echo $ics;
?>

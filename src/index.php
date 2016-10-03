<?php
	if (array_key_exists('action', $_GET)) {
		$action = $_GET['action'];
		if ($action === 'url' || $action === 'download') {
			include('timetable.php');
		}
	}
?>
<!DOCTYPE HTML>
<html>
	<head>
		<title>Timetable</title>
		<style>
			ul {
				list-style-type: none;
			}
		</style>
	</head>
	<body>
		<form action=".">
			Courses: <ul>
<?php
	$courses_data = json_decode(file_get_contents('courses_data.json'), true);
	for ($i = 0; $i <= 17; ++$i) {
		$number = ($i < 10 ? '0' . $i : '' . $i);
		$crx = 'CR'.$number;
		$course_name = $courses_data[$crx]['name'];
		$checked = array_key_exists($crx, $_GET) && $_GET[$crx] === 'on' ? ' checked=\"checked\"' : '';
		print("				<li><label><input type=\"checkbox\" name=\"$crx\"$checked>CR$number: $course_name</label></li>\n");
	}
?>
			</ul>
			<button type="submit">Remember choices (in URL)</button>
			<button type="submit" name="action" value="url" onclick="this.form.action='timetable.php';">Get ICS URL</button>
			<button type="submit" name="action" value="download" onclick="this.form.action='timetable.php';">Download ICS</button>
		</form>
	</body>
</html>

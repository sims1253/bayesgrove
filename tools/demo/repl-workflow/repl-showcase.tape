Output tools/demo/repl-workflow/repl-showcase.mp4

Require R

Set Shell "bash"
Set FontSize 18
Set Width 1200
Set Height 800
Set Padding 25
Set Framerate 15
Set PlaybackSpeed 1
Set Theme "GruvboxDark"
Set WindowBar Colorful
Set TypingSpeed 25ms

Hide

Type "R --quiet"
Sleep 1s
Enter
Sleep 1s

Type 'options(bg_demo_autostart = FALSE); source("tools/demo/repl-workflow/launch-demo.R"); demo_repl_showcase(start_repl = TRUE)'
Sleep 500ms
Enter
Sleep 3s

Show

Sleep 2s
Type "gates"
Sleep 1s
Enter
Sleep 3s

Sleep 2s

Type "answer"
Sleep 1s
Enter
Sleep 1s
Type "1"
Sleep 1s
Enter
Sleep 1s
Type "Only 5% of rows were dropped, so the cleaned data are still usable."
Sleep 1s
Enter
Sleep 3s

Type "run"
Sleep 1s
Enter
Sleep 4s

Type 'result "Baseline Fit"'
Sleep 1s
Enter
Sleep 6s

Hide

Type "exit"
Sleep 1s
Enter
Sleep 1s

Type 'q("no")'
Sleep 1s
Enter

Show
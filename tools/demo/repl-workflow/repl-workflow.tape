Output tools/demo/repl-workflow/repl-workflow.mp4

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

Type 'options(bg_demo_autostart = FALSE); source("tools/demo/repl-workflow/launch-demo.R"); demo_repl_workflow(start_repl = TRUE)'
Sleep 1s
Enter
Sleep 3s

Show

Sleep 2s

Type "status"
Sleep 1s
Enter
Sleep 3s

Type 'result "Alternative: Robust Prior"'
Sleep 1s
Enter
Sleep 4s

Type "guide"
Sleep 1s
Enter
Sleep 4s

Type "do 2"
Sleep 1s
Enter
Sleep 1s

Type "Y"
Sleep 1s
Enter
Sleep 1s

Type "Alternative: Robust Prior (fixed)"
Sleep 1s
Enter
Sleep 1s

Type "Y"
Sleep 1s
Enter
Sleep 3s

Type "run"
Sleep 1s
Enter
Sleep 3s

Type 'result "Alternative: Robust Prior (fixed)"'
Sleep 1s
Enter
Sleep 4s

Type 'retire "Alternative: Robust Prior"'
Sleep 1s
Enter
Sleep 3s

Type "guide"
Sleep 1s
Enter
Sleep 4s

Type "do 1"
Sleep 1s
Enter
Sleep 1s

Type "Y"
Sleep 1s
Enter
Sleep 1s

Type "Compare Baseline vs Revision"
Sleep 1s
Enter
Sleep 3s

Type "run"
Sleep 1s
Enter
Sleep 4s

Type 'result "Compare Baseline vs Revision"'
Sleep 1s
Enter
Sleep 4s

Type "guide"
Sleep 1s
Enter
Sleep 3s

Type "do 1"
Sleep 1s
Enter
Sleep 1s

Type "Y"
Sleep 1s
Enter
Sleep 1s

Type "prefer_revised_branch"
Sleep 1s
Enter
Sleep 1s

Type "The robust prior model has significantly better predictive performance (ELPD diff 14.2, SE 4.5)."
Sleep 1s
Enter
Sleep 3s

Type "guide"
Sleep 1s
Enter
Sleep 4s

Type "do 1"
Sleep 1s
Enter
Sleep 1s

Type "Y"
Sleep 1s
Enter
Sleep 1s

Type "1"
Sleep 1s
Enter
Sleep 1s

Type "Promote the revised branch as the accepted analysis path."
Sleep 1s
Enter
Sleep 3s

Type "status"
Sleep 1s
Enter
Sleep 4s

Hide

Type "exit"
Sleep 1s
Enter
Sleep 1s

Type 'q("no")'
Sleep 1s
Enter

Show
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
Set TypingSpeed 50ms

Type "R --quiet"
Sleep 1s
Enter
Sleep 1s

Type 'source("tools/demo/repl-workflow/launch-demo.R")'
Sleep 1s
Enter

# Wait for setup (fit runs, produces warning summary)
Sleep 3s

Type "# 1. Check workflow state - blocked by protocol"
Enter
Sleep 500ms
Type "status"
Sleep 1s
Enter
Sleep 2s

Type "# 2. See what the protocol engine requires"
Enter
Sleep 500ms
Type "guide"
Sleep 1s
Enter
Sleep 3s

Type "# 3. View actionable suggested actions"
Enter
Sleep 500ms
Type "actions"
Sleep 1s
Enter
Sleep 3s

Type "# 4. Inspect the fit to understand the warning"
Enter
Sleep 500ms
Type 'result "Fit Centered Parametrization"'
Sleep 1s
Enter
Sleep 3s

Type "# 5. Branch and modify to fix divergences"
Enter
Type "#    Action 2 suggests non-centered parametrization"
Enter
Sleep 500ms
Type "do 2"
Sleep 1s
Enter
Sleep 1s

# Enter label for the branch
Type "Fit Non-Centered"
Sleep 500ms
Enter
Sleep 500ms

# Apply suggested parameter changes
Type "Y"
Sleep 500ms
Enter
Sleep 2s

Type "# 6. The branch is created with clean params"
Enter
Type "#    Now run it to verify the fix"
Enter
Sleep 500ms
Type "run"
Sleep 1s
Enter
Sleep 4s

Type "# 7. Check the new fit's diagnostics"
Enter
Sleep 500ms
Type 'result "Fit Non-Centered"'
Sleep 1s
Enter
Sleep 3s

Type "# 8. The original fit still has warning diagnostics"
Enter
Type "#    Invalidate it to clear the obligation"
Enter
Sleep 500ms
Type 'invalidate "Fit Centered Parametrization"'
Sleep 1s
Enter
Sleep 2s

Type "# 9. Protocol obligation is now resolved"
Enter
Sleep 500ms
Type "guide"
Sleep 1s
Enter
Sleep 2s

Type "# 10. View recent decisions made during the session"
Enter
Sleep 500ms
Type "decisions"
Sleep 1s
Enter
Sleep 2s

Type "# 11. Workflow is now healthy"
Enter
Sleep 500ms
Type "status"
Sleep 1s
Enter
Sleep 2s

Type "# 12. See the final graph with the branch"
Enter
Sleep 500ms
Type "nodes"
Sleep 1s
Enter
Sleep 3s

Type "exit"
Sleep 1s
Enter
Sleep 1s

Type 'q("no")'
Sleep 1s
Enter

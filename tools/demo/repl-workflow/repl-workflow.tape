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

# Wait for setup (clean baseline fit + warning branch)
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

Type "# 4. Inspect the branch fit to understand the warning"
Enter
Sleep 500ms
Type 'result "Fit Centered Parametrization"'
Sleep 1s
Enter
Sleep 3s

Type "# 5. Branch and modify the problematic fit"
Enter
Type "#    Action 2 suggests a non-centered revision"
Enter
Sleep 500ms
Type "do 2"
Sleep 1s
Enter
Sleep 1s

# Enter label for the revised branch
Type "Fit Non-Centered Revision"
Sleep 500ms
Enter
Sleep 500ms

# Apply suggested parameter changes
Type "Y"
Sleep 500ms
Enter
Sleep 2s

Type "# 6. The revised branch is created with clean params"
Enter
Type "#    Now run it to verify the fix"
Enter
Sleep 500ms
Type "run"
Sleep 1s
Enter
Sleep 4s

Type "# 7. Check the revised fit's diagnostics"
Enter
Sleep 500ms
Type 'result "Fit Non-Centered Revision"'
Sleep 1s
Enter
Sleep 3s

Type "# 8. Retire the stale warning branch"
Enter
Sleep 500ms
Type 'retire "Fit Centered Parametrization"'
Sleep 1s
Enter
Sleep 2s

Type "# 9. Review the comparison obligation"
Enter
Sleep 500ms
Type "guide"
Sleep 1s
Enter
Sleep 2s

Type "# 10. With two clean fits, create the comparison node"
Enter
Sleep 500ms
Type "do 1"
Sleep 1s
Enter
Sleep 1s

# Comparison label
Type "Compare Baseline vs Revision"
Sleep 500ms
Enter
Sleep 2s

Type "# 11. Run the comparison node"
Enter
Sleep 500ms
Type "run"
Sleep 1s
Enter
Sleep 4s

Type "# 12. Review the comparison decision action"
Enter
Sleep 500ms
Type "guide"
Sleep 1s
Enter
Sleep 2s

Type "# 13. Record the model comparison decision"
Enter
Sleep 500ms
Type "do 1"
Sleep 1s
Enter
Sleep 1s

# Model comparison choice
Type "prefer_revised_branch"
Sleep 500ms
Enter
Sleep 500ms

# Model comparison rationale
Type "The revised branch resolves the diagnostic warning cleanly."
Sleep 500ms
Enter
Sleep 2s

Type "# 14. Review the remaining branch obligation"
Enter
Sleep 500ms
Type "guide"
Sleep 1s
Enter
Sleep 2s

Type "# 15. Explicitly accept the revised branch"
Enter
Sleep 500ms
Type "do 1"
Sleep 1s
Enter
Sleep 1s

# Accept the branch
Type "1"
Sleep 500ms
Enter
Sleep 500ms

# Branch disposition rationale
Type "Promote the revised branch as the accepted analysis path."
Sleep 500ms
Enter
Sleep 2s

Type "# 16. The workflow is now healthy"
Enter
Sleep 500ms
Type "guide"
Sleep 1s
Enter
Sleep 2s

Type "# 17. Check overall status"
Enter
Sleep 500ms
Type "status"
Sleep 1s
Enter
Sleep 2s

Type "# 18. See the final graph with the comparison and accepted branch"
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

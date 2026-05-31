# Prompts

## 2026-04-29

1. Analyze the existing code and create a CLAUDE.md file for Claude Code.

## 2026-04-30

2. Re-analyze changed file config.org and update CLAUDE.md if needed.

3. check file init.el for errors

4. implement the fix to remove the eval-when-compile and package-initialize
   calls.

5. add recent prompts to PROMPTS.md

6. check @config.org for errors

7. fix the five definite bugs in config.org

8. re-check @config.org

9. Implement a change to add the my_emacs/ directory to the load path, and
   configure emacs to omit the .github directory and its descendants.

10. Undo the change to treemacs, and add a configuration to omit .github/
    directory from load-path.

11. undo the change to ido

12. Why does the treemacs sidebar disappear when emacs starts?

13. yes

14. Rewrite the get-config-file to properly expand to the my_emacs directory.
    Fix the the keybinding on line 148. add the :commands line to elpy use-
    package. Replace the :custom on line 308 with the correct :config clause.
    Update the gptel-model setting to use Claude Sonnet 4.6.

15. check @early-init.org for errors.

16. yes, and update the one-armed if calls.

17. check for installed emacs packages that don't have use-package declarations,
    and add the equivalent use-package calls to config.org

18. evaluate all the keybindings in my config files and report conflicts with
    each other or with emacs built-ins

19. update CLAUDE.md to reflect current project state, including external
    changes to files

20. update CLAUDE.md to remove references to removed files lisp/site.macos.el
    and lisp/site.linux.el.

21. clean up configuration for python to always use mise and uv for environment
    setup.

22. update the configuration to use markdown-mode for markdown files, not
    markdown-ts-mode

23. Fix the cause of the error message I received opening a markdown file:
    "redisplay--pre-redisplay-functions: (treesit-query-error "Node type error
    at" 2 "(inline) @markdown-inline" "Debug the query with `treesit-query-
    validate'") ..."

24. evaluate the following error and determine why it still occurs: [backtrace
    showing markdown-ts-mode still active via treesit-auto recipe list]

## 2026-05-03

25. perform code review on @/Users/adrianflanagan/.config/emacs/my_emacs/early-
    config.org and @/Users/adrianflanagan/.config/emacs/my_emacs/config.org and
    recommend fixes and improvements.

26. save this response to file REVIEW.md

27. modify the list of items in REVIEW.md so that each item has a checkbox which
    can be checked when that item is implemented.

28. write a .dir-locals.el file to require the use of gfm-mode when editing
    REVIEW.md

## 2026-05-05

29. write a new file REVIEW_LEFT.md containing the items from @REVIEW.md which
    do not have an "x" in the checkbox at the beginning of the item.

30. Redo the search for mixed setq/setopt options (review item #24), then update
    @REVIEW.md and @REVIEW_LEFT.md with the new line numbers.

31. Revert changes to the two files. Instead, mark review item 24 as done, by
    checking the box.

32. review file @REVIEW.md; insure the prompts are in proper date order, and
    rearrange date sections if they are not.

33. remove the numbers from the list items in the last date section in
    @PROMPTS.md

34. rewrite the last section of @PROMPTS.md and combine the two -- i.e. write
    the first prompt with filenames corrected, and remove the final prompt.

35. summarize the contents of project memory

36. remember also to wrap commit message lines at 80 characters, and save the
    instructions for @PROMPTS.md to my global Claude configuration.

37. re-read ~/.claude/CLAUDE.md, and use the instructions there to reformat the
    project @PROMPTS.md for readability.

38. remember that ordered lists in @PROMPTS.md should not be numbered, and
    remove the numbering from the file.

39. write a configuration for kirigami such that it only effects buffers which
    do not have tree-sitter parsers in use.

## 2026-05-09

40. Analyze code and update CLAUDE.md as needed.

41. update @CLAUDE.md to reflect removal of prettier-js

42. cross-reference @custom.el and @config.org and list any installed packages
    that don't have use-package declarations in @config.org

43. add a section called "CMake" to the @config.org file and write use-package
    statements for all three cmake packages. Include any custom settings defined
    in the use-package statements.

44. update @CLAUDE.md

45. remember markdown-ts-mode is a built-in package for emacs.

46. Remove packages copilot, highlight-parantheses, casual-info, and casual-lib
    settings from files.

## 2026-05-13

47. Write an emacs lisp function to read the file
    @/Users/adrianflanagan/.config/emacs/my_emacs/config.org, and print out a
    list of packages installed via the (use-package) calls in the file.

48. Update function my/list-use-packages in @lisp/my-misc.el to ignore any line
    beginning with (optional) whitespace and a semicolon.

## 2026-05-31

49. Re-read changed files and update CLAUDE.md to reflect changes. Keep the
    level of detail about the same as the current file.

50. remember to order prompts in @PROMPTS.md in ascending order by date. Then
    update the file so that all prompts are listed in the correct order.

51. why doesn't @/Users/adrianflanagan/.config/emacs/my_emacs/CLAUDE.md
    reference package org-modern-mode?

52. Compare @/Users/adrianflanagan/.config/emacs/my_emacs/config.org and
    @/Users/adrianflanagan/.config/emacs/my_emacs/CLAUDE.md and add any packages
    which are missing from
    @/Users/adrianflanagan/.config/emacs/my_emacs/CLAUDE.md to that file.

53. Add a number prefix to each paragraph in
    @/Users/adrianflanagan/.config/emacs/my_emacs/PROMPTS.md, for example 1st
    paragraph should have "1.  " and the third paragraph should have "3. " --
    don't restart numbering for each date.

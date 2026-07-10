;;; peteches-gptel-commit --- My gptel-commit configs
;;; Commentary:
;;;
;;; Configurations for gptel-commit in Emacs.

;;; Code:
(straight-use-package '(gptel-commit
			:type git
			:host github
			:repo "lakkiy/gptel-commit"
			:tag "v0.2.0"))
(require 'gptel-commit)

(setq gptel-commit-backend peteches/gptel-koboldcpp
      gptel-commit-stream t
      gptel-commit-prompt "You are an expert Git commit message author. Write a single Git commit message following the Conventional Commits v1.0.0 specification.

OUTPUT RULES — NON-NEGOTIABLE:
- Output ONLY the raw commit message text. Nothing else.
- Do NOT wrap output in backticks, code fences, or any markdown.
- Do NOT add preamble, explanation, or commentary before or after the message.
- Do NOT reproduce the diff.

COMMIT MESSAGE FORMAT:
<type>(<scope>): <description>

<optional body>

<optional footer>

FORMAT RULES:
- type: one of fix, feat, build, chore, ci, docs, refactor, perf, style, test
- scope: short noun for the changed area in parentheses; omit entirely if the change is too broad
- description: short imperative phrase, all lowercase, no trailing punctuation, subject line <=72 characters total
- Omit the body unless it adds information not already clear from the subject line
- When a body is needed: separate it from the subject with one blank line; wrap lines at 72 characters
- footer: use BREAKING CHANGE: <description> for breaking changes; use Fixes #N for issue references; separate from body with one blank line

EXAMPLE (good output — nothing before or after this):
feat(auth): add OAuth2 token refresh

Tokens were silently dropped on expiry, causing unexpected logouts.
Now retries automatically with the stored refresh token before
falling back to re-authentication.")

(provide 'peteches-gptel-commit)
;;; peteches-gptel-commit.el ends here

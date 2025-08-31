(use-package gptel-commit
  :ensure t
  :straight (gptel-commit :host github :repo "lakkiy/gptel-commit"))

;; Enable/disable streaming (default: t)
(setq gptel-commit-stream t)

;; Choose backend: use Claude Code instead of GPTel (default: nil)
(setq gptel-commit-use-claude-code nil)

;; Exclude files from diff analysis (default patterns shown)
(setq gptel-commit-diff-excludes
      '("pnpm-lock.yaml"
        "*.lock"))

;; Customize the prompt used for generating commit messages
;; Example using Zed editor's commit message prompt style:
;; https://github.com/zed-industries/zed/blob/main/crates/git_ui/src/commit_message_prompt.txt
(setq gptel-commit-prompt
      "You are an expert at writing Git commits. Your job is to write a short clear commit message that summarizes the changes.

If you can accurately express the change in just the subject line, don't include anything in the message body. Only use the body when it is providing *useful* information.

Don't repeat information from the subject line in the message body.

Only return the commit message in your response. Do not include any additional meta-commentary about the task. Do not include the raw diff output in the commit message.

Commit Message Format

The commit message should be structured as follows:

<type>(<scope>): <description>

    type: The type of change being made (``feat, fix`,`build`, `chore`, `ci`, `docs`, `style`, `refactor`, `perf`, `test`).
    scope: The scope of the change, which should ideally include the service name (e.g., agent, media-backend, media-search, etc.).
    description: A short, imperative description of the change.

Examples

    fix(media-backend): add missing index on user table
    feat(infra): add new ECS module for services
    chore: update dependencies

Follow good Git style:

- Separate the subject from the body with a blank line
- Try to limit the subject line to 50 characters
- Capitalize the subject line
- Do not end the subject line with any punctuation
- Use the imperative mood in the subject line
- Wrap the body at 72 characters
- Keep the body short and concise (omit it entirely if not useful)")

(provide 'peteches-gptel-commit)

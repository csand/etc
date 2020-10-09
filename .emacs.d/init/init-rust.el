;;; init-rust.el --- Rust IDE packages, also see init-lsp.el

(use-package rustic
  :hook (rustic-mode-hook . flycheck-mode))

(use-package flycheck-rust
  :after rustic
  :hook (flycheck-mode-hook . flycheck-rust-setup))

(provide 'init-rust)

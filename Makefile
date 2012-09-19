
WORKING_DIR = $(PWD)
REPO = $(HOME)/.files
REPO_URL = https://github.com/coarsesand/dotfiles.git
LN = ln -sf
RM = rm

objects = $(HOME)/.zshrc $(HOME)/.zshenv $(HOME)/.oh-my-zsh $(HOME)/.vim $(HOME)/.vimrc\
		  $(HOME)/.hgrc $(HOME)/.mercurial $(HOME)/.tmux.conf $(HOME)/.gitconfig\
		  $(HOME)/.bin


.PHONY : links repo clean

# Phony actions, use these for most commands
links: $(REPO) $(objects)
	@echo "Symlinks created successfully"

repo: $(REPO)

clean:
	@$(RM) $(objects)
	@echo "Removed all links"

#
$(REPO):
	@git clone $(REPO_URL) $(REPO)
	@cd $(REPO)
	@git submodule init
	@git submodule update
	@cd $(WORKING_DIR)
	@echo "Dot files repository cloned to ~/.files"

$(HOME)/.zshrc: $(REPO)
	@$(LN) $(REPO)/zsh/zshrc $(HOME)/.zshrc
	@echo "Linked .zshrc"

$(HOME)/.zshenv: $(REPO)
	@$(LN) $(REPO)/zsh/zshenv $(HOME)/.zshenv
	@echo "Linked .zshenv"

$(HOME)/.oh-my-zsh/: $(REPO)
	@$(LN) $(REPO)/zsh/oh-my-zsh/ $(HOME)/.oh-my-zsh
	@echo "Linked oh-my-zsh"

$(HOME)/.vimrc: $(REPO)
	@$(LN) $(REPO)/vim/vimrc $(HOME)/.vimrc
	@echo "Linked .vimrc"

$(HOME)/.vim/: $(REPO)
	@$(LN) $(REPO)/vim/ $(HOME)/.vim
	@echo "Linked vim folder"
	@vim +BundleInstall +qall
	@echo "All vim bundles installed"

$(HOME)/.gitconfig: $(REPO)
	@$(LN) $(REPO)/git/gitconfig $(HOME)/.gitconfig
	@echo "Linked .gitconfig"

$(HOME)/.hgrc: $(REPO)
	@$(LN) $(REPO)/mercurial/hgrc $(HOME)/.hgrc
	@echo "Linked .hgrc"

$(HOME)/.mercurial/: $(REPO)
	@$(LN) $(REPO)/mercurial/ $(HOME)/.mercurial
	@echo "Linked mercurial folder"

$(HOME)/.tmux.conf: $(REPO)
	@$(LN) $(REPO)/tmux/tmux.conf $(HOME)/.tmux.conf
	@echo "Linked tmux config"

$(HOME)/.bin: $(REPO)
	@$(LN) $(REPO)/scripts $(HOME)/.bin
	@echo "Linked scripts as ~/.bin"


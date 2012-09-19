
REPO_URL = 'git://github.com/coarsesand/dotfiles.git'

HOME = ENV['HOME']
REPO = "#{HOME}/.files"

DOTFILES = {
  "zsh/zshrc"      => ".zshrc",
  "zsh/zshenv"     => ".zshenv",
  "zsh/oh-my-zsh/" => ".oh-my-zsh",
  "vim/"           => ".vim",
  "vim/vimrc"      => ".vimrc",
  "git/gitconfig"  => ".gitconfig",
  "mercurial/hgrc" => ".hgrc",
  "mercurial/"     => ".mercurial",
  "tmux/tmux.conf" => ".tmux.conf",
  "scripts/"       => ".bin"
}

def git_clone(url, dest = "")
  sh "git clone #{url} #{dest}"
end

def home(file)
  "#{HOME}/#{file}"
end

def repo(file)
  "#{REPO}/#{file}"
end

def repo_files
  files = FileList.new
  DOTFILES.each do |file, _|
    files.add repo(file)
  end
  files
end

def dotfiles
  files = FileList.new
  DOTFILES.each do |_, file|
    files.add home(file)
  end
  files
end

repo_files.zip(dotfiles).each do |src, dest|
  file dest => src do
    ln_sf src, dest
  end
end

file REPO do
  git_clone REPO_URL, REPO
end

task :default => :setup

desc 'Clones the dotfiles repository, creates links, and fetches plugins'
task :setup => [REPO, 'dot:links' 'vim:install']

namespace :dot do
  task :links => dotfiles do
    puts "\n=== Symlinks created ===\n\n"
  end
end


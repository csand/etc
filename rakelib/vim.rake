PLUGIN_FILE = 'vim/plugins'
BUNDLE = "#{REPO}/vim/bundle"

def git_clone(url, dest = "")
  sh "git clone #{url} #{dest}"
end

def git_pull
  sh "git pull"
end

def bundle(file = "")
  "#{BUNDLE}/#{file}"
end

def github_url(plugin)
  "https://github.com/#{plugin}.git"
end

def vimscript_url(plugin)
  "https://github.com/vim-scripts/#{plugin}.git"
end

def vim_plugins
  plugins = {}
  source = nil
  File.open PLUGIN_FILE do |f|
    f.each do |line|
      case line.strip
      when '[github]'
        source = :github
      when '[vimscripts]'
        source = :vimscripts
      when ''
        source = nil
      else
        plugin = line.strip
        case source
        when :github
          name = plugin.split('/')[1]
          plugins[name] = github_url(plugin)
        when :vimscripts
          plugins[plugin] = vimscript_url(plugin)
        end
      end
    end
  end
  plugins
end

def installed_plugins
  Dir.foreach(BUNDLE).drop(2)
end

def missing_plugins
  missing = {}
  vim_plugins.each do |name, url|
    unless installed_plugins.include? name
      missing[name] = url
    end
  end
  missing
end

namespace :vim do

  desc 'Install vim plugins to the bundle directory'
  task :install do
    missing_plugins.each do |name, url|
      git_clone url, bundle(name)
      puts "Installed #{name}"
    end
    puts "\n=== Vim plugins installed ===\n\n"
  end

  desc 'List installed vim plugins'
  task :list do
    installed_plugins.each do |plugin|
      puts plugin
    end
  end

  desc 'Update installed vim plugins'
  task :update do
    installed_plugins.each do |name, _|
      cd bundle(name) do
        git_pull
        puts "#{name} updated"
      end
    end
    puts "\n=== Vim plugins updated ===\n\n"
  end

  desc 'Remove all installed vim plugins'
  task :remove do
    installed_plugins.each do |plugin|
      cd bundle do
        rm_rf plugin
      end
    end
  end
end


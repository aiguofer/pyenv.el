desc "create a new release"
task 'release' do
  current_version = run('git tag').split(/\n/).last.strip[1..-1]
  puts "What version do you want to release? (current: #{current_version})"
  version = STDIN.gets.strip
  version_tag = "v%s" % version

  if run('git tag').split(/\n/).include?(version_tag)
    raise("This tag has already been committed to the repo.")
  end

  rbenv_contents = File.read('rbenv.el')
  File.write('rbenv.el', rbenv_contents.gsub("Version: #{current_version}", "Version: #{version}"))

  run "git commit -a -m \"prepare #{version}\""

  run "git tag -a -m \"Version #{version}\" #{version_tag}"
  run "git push origin"
  run "git push origin --tags"
end

def run(command)
  `#{command}`
end

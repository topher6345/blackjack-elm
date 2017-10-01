task default: %w[build]

task :build do
  sh 'elm make src/Main.elm --output elm.js'
  inline_css
end

def inline_css
  file = '_index.html'
  main_css = File.read('css/main.css')
  old_contents = File.read(file)
  new_contents = old_contents.gsub(
    "<link href='css/main.css' rel='stylesheet'>",
    "<style>\n#{main_css}\n</style>",
  )
  File.open('index.html', 'w') { |newfile| newfile.write(new_contents) }
end

require 'base64'
task default: %w[build]

task :build do
  sh 'elm make src/Main.elm --output js/elm.js'
end

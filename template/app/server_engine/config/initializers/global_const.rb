FRAMEWORK_ROOT_DIR = File.dirname(File.dirname(Rails.root.to_s))

def check_to_write(path, content)
  if File.exists?(path)
    md5_value = Digest::MD5.hexdigest(File.open(path, "r"){|io| io.read}.strip)
  end
  unless Digest::MD5.hexdigest(content.strip) == md5_value
    File.open(path, 'w'){|io| io.write content}
  end
end

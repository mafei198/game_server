class SchemaPersistance < ActiveRecord::Base
  attr_accessible :uuid, :version
end

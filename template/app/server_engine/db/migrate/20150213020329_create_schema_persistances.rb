class CreateSchemaPersistances < ActiveRecord::Migration
  def change
    create_table :schema_persistances, id: false do |t|
      t.string :uuid
      t.integer :version
    end

    add_index :schema_persistances, :uuid, unique: true
  end
end

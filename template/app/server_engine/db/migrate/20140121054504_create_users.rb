class CreateUsers < ActiveRecord::Migration
  def change
    create_table :users, id: false do |t|
      t.string :uuid
      t.string :udid
      t.string :name
      t.string :locale
      t.string :device_token
    end

    add_index :users, :uuid, unique: true
    add_index :users, :udid, unique: true
    add_index :users, :name, unique: true
  end
end

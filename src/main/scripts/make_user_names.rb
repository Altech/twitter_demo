#! /usr/local/bin/ruby
# coding: utf-8


require 'json'

mapping = Hash.new # user_id to name
required_users = Array.new

topn_data = File.open('resources/10users_topn_100.json','r')
mapping_data = File.open('resources/alluserlist','r')

out = File.open('resources/10users_topn_100_userlist.tsv','w')


topn_data.each do |l|
  json = JSON.parse(l)
  ids = json["Result"]["Topk"]
  required_users += ids
end


required_users.uniq!
required_users.sort!

name_data = Hash.new


mapping_data.each_with_index do |l,i|
  puts "now: #{i}" if i % 100000 == 0 
  id, name = l.split(/[\s]+/).map(&:strip)
  id = id.to_i
  name_data[id] = name
  
  if i%1000000 == 0
    required_users.each do |id|
      if name_data.has_key? id
        out.puts "#{id}\t#{name_data[id]}"
      end
    end
    name_data = Hash.new
  end
end

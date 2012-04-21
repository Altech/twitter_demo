#! /usr/local/bin/ruby
# coding: utf-8

require 'json'

mapping = Array.new # edge_id to twitter_id
seq_to_time = Array.new
converted_json = Array.new # edge_id to twitter_id

mapping_data = File.open('dataset/user_sort.csv','r')
time_data = File.open('dataset/input.csv','r')
prox_data = File.open('dataset/topn2.json','r')

# make mapping
mapping_data.each do |l|
  edge_id, twitter_id = l.split(',').map(&:to_i)
  mapping[edge_id] = twitter_id
end

# get time
time_data.each_with_index do |l,i|
  time = l.split(',')[3]
  seq_to_time[i] = time
end

# convert id
prox_data.each do |l|
  json = JSON.parse(l)
  next if not json.include? 'Sequence'
  next unless json['Result']['Sample'] == 2238611 # FIXME

  json['Time'] = seq_to_time[json['Sequence'].to_i+7872371] # [FIXME] 固定値は始まり時点のSequence.
  result = json['Result']
  result['Sample'] = mapping[result['Sample']]
  result['Topk'] = result['Topk'][1..10].map{|id| mapping[id]}
  
  converted_json << json
end

mapping_data.close
prox_data.close


# write data
converted_json.each do |json|
  puts json.to_json
end


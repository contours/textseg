#!/usr/bin/env python2
##
## Dump all interview text from "contours/segment"-format Redis database to plain text files
## in the current directory. One sentence per line.
##

import redis

db = redis.StrictRedis(host='localhost',port=6379,db=0)
interview_ids = db.smembers('interviews')
for interview_id in interview_ids:
    print interview_id
    fp = file(interview_id.lstrip('interviews:') + '.txt', 'w')
    sentence_ids = db.lrange(interview_id+':sentences',0,-1)
    for sentence_id in sentence_ids:
        text = db.hget(sentence_id, 'text')
        fp.write(text)
        fp.write("\n")


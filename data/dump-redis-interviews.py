#!/usr/bin/env python
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
        text = db.hget(sentence_id, 'text').decode('utf8')
        text = text.translate({
            0x2013: ord('-'), # en dash
            0x2014: ord('-'), # em dash
            0x2018: ord("'"), # left single quote
            0x2019: ord("'"), # right single quote
            0x2032: ord("'"), # prime
            0x2033: ord('"'), # double prime
            0x201c: ord('"'), # left double quote
            0x201d: ord('"'), # right double quote
            0x2026: u"...",   # ellipsis
            0x07de: ord('-')  # a very strange character which must be an error
            })
        fp.write(text.encode('utf8'))
        fp.write("\n")


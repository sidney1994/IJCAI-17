'''myfile = open("F:\userpay.txt","r") 
#lines = len(myfile.readlines()) 
#print "There are %d lines in %s" % (lines, filename)
for line in myfile.readlines()[0:1]:
    print line
'''
import datetime
import pickle

start = datetime.datetime.now()
print "start--%s"  % (start)

fileHandle = open ( 'F:\userpay.txt','r')#'''
file2 = open('F:\data8000000.txt','w')

i = 0
while ( i < 80000000):
    a = fileHandle.readline()
    if(i>=70000000):
        file2.write(''.join(a)) 
    i = i + 1

fileHandle.close() 
file2.close()

print "done--%s" % ( datetime.datetime.now() - start)

if __name__ == '__main__':
    pass

fileHandle.close()
file2.close()


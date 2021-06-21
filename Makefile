# Get a copy of the data
# The MO's cert has currently expired, hence the --insecure option
#

.PHONY: clean

cetml1659on.dat:
	curl -O --insecure https://hadobs.metoffice.com/hadcet/cetml1659on.dat

clean:
	rm -f cetml1659on.dat *~

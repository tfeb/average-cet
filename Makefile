# Get copies of the data files
#

.PHONY: data-files clean

DATAFILES = meantemp_daily_totals.txt meantemp_monthly_totals.txt

LEGACYFILES = cetml1659on.dat cetdl1772on.dat

data-files: $(DATAFILES)

legacy-files: $(LEGACYFILES)

meantemp_monthly_totals.txt:
	curl -O https://www.metoffice.gov.uk/hadobs/hadcet/data/meantemp_monthly_totals.txt

cetml1659on.dat:
	curl -O https://www.metoffice.gov.uk/hadobs/hadcet/legacy/data/cetml1659on.dat

meantemp_daily_totals.txt:
	curl -O https://www.metoffice.gov.uk/hadobs/hadcet/data/meantemp_daily_totals.txt

cetdl1772on.dat:
	curl -O https://www.metoffice.gov.uk/hadobs/hadcet/legacy/data/cetdl1772on.dat


clean:
	rm -f $(DATAFILES) $(LEGACYFILES) *~

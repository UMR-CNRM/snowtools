ln -sf $EXESURFEX/../STRATO/TESTS/ISBA/drdt_bst_fit_60.nc $SNOWTOOLS_CEN/tests/test_soda/drdt_bst_fit_60.nc
ln -sf $EXESURFEX/../MY_RUN/ECOCLIMAP/ecoclimapI_covers_param.bin $SNOWTOOLS_CEN/tests/test_soda/ecoclimapI_covers_param.bin
ln -sf $EXESURFEX/../MY_RUN/ECOCLIMAP/ecoclimapII_eu_covers_param.bin $SNOWTOOLS_CEN/tests/test_soda/ecoclimapII_eu_covers_param.bin

for i in {1..8}
do 
  ln -sf $SNOWTOOLS_CEN/tests/test_soda/PREP.nc PREP_140225H06_PF_ENS$i.nc
done

ln -sf $EXESURFEX/SODA soda.exe

./soda.exe

unlink drdt_bst_fit_60.nc
unlink ecoclimapI_covers_param.bin
unlink ecoclimapII_eu_covers_param.bin
unlink soda.exe
for i in {1..8}
do 
  unlink PREP_140225H06_PF_ENS$i.nc
done

rm -f SURF*
rm -f PART
rm -f ALPHA
rm -f BG_CORR
rm -f gmon.out
rm -f IMASK
rm -f ISBA*
rm -f LISTING*
rm -f log0

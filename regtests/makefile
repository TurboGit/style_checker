
.SILENT:

YEAR=$(shell date +"%Y")

all: run_tests check_results

run_tests:
	-../style_checker -H error1.txt demo.adb > out1.res 2>&1
	-../style_checker -HLSBCT error1.txt demo.adb > out2.res 2>&1
	-../style_checker -lang Ada -HS error1.txt demo.adb > out3.res 2>&1
	-../style_checker -HL error1.txt azerty.adb > out4.res 2>&1
	-../style_checker -HBCELT -lang Ada main.adb > out5.tmp 2>&1
	-../style_checker -HBCELT -lang Ada -cy main.adb > out6.tmp 2>&1
	-../style_checker -HBCELT -lang Ada -cp main.adb > out7.tmp 2>&1
	sed "s,$(YEAR),2008,g" out5.tmp > out5.res
	sed 's,$(YEAR),2008,g' out6.tmp > out6.res
	sed 's,$(YEAR),2008,g' out7.tmp > out7.res
	-../style_checker -H endings.txt > out8.res 2>&1
	-../style_checker -H noendings.txt > out9.res 2>&1
	-../style_checker -HA hello.e > out10.res 2>&1
	-../style_checker -ign e hello.e >> out10.res 2>&1
	-../style_checker -A -m 2 hello.e > out11.res 2>&1
	-../style_checker header1.adb header2.adb header3.adb > out12.tmp 2>&1
	sed 's,$(YEAR),2010,g' out12.tmp > out12.res
	sed 's,2005,$(YEAR),g' copyright.ads > copyright2.ads
	-../style_checker -S -cY -H \
		-cf'Copyright \(C\) 2\d\d\d-2\d\d\d, (Pascal Obry|Qwerty)' \
		copyright2.ads > out13.res 2>&1
	-../style_checker -H script.sh > out14.res 2>&1
	-../style_checker -d -cY pck.ads > out15.res 2>&1
	-../style_checker -d -cY main.c > out16.res 2>&1
	-../style_checker -d -cY code.c > out17.res 2>&1
	-../style_checker -H withtab.adb > out18.res 2>&1
	-echo a >> out18.res
	-../style_checker -Ha withtab.adb >> out18.res 2>&1
	-echo A >> out18.res
	-../style_checker -HA withtab.adb >> out18.res 2>&1
	-../style_checker -C makefile > out19.res 2>&1
	-../style_checker -Ho operator.adb > out20.res 2>&1
	-../style_checker -Hi thenlayout.adb > out21.res 2>&1

check_results:
	echo "### regressions follows"
	rm -f out_status.res
	-diff -wc out1.out out1.res || echo $$? >> out_status.res
	-diff -wc out2.out out2.res || echo $$? >> out_status.res
	-diff -wc out3.out out3.res || echo $$? >> out_status.res
	-diff -wc out4.out out4.res || echo $$? >> out_status.res
	-diff -wc out5.out out5.res || echo $$? >> out_status.res
	-diff -wc out5.out out6.res || echo $$? >> out_status.res
	-diff -wc out5.out out7.res || echo $$? >> out_status.res
	-diff -wc out8.out out8.res || echo $$? >> out_status.res
	-diff -wc out9.out out9.res || echo $$? >> out_status.res
	-diff -wc out10.out out10.res || echo $$? >> out_status.res
	-diff -wc out11.out out11.res || echo $$? >> out_status.res
	-diff -wc out12.out out12.res || echo $$? >> out_status.res
	-diff -wc out13.out out13.res || echo $$? >> out_status.res
	-diff -wc out14.out out14.res || echo $$? >> out_status.res
	-diff -wc out15.out out15.res || echo $$? >> out_status.res
	-diff -wc out16.out out16.res || echo $$? >> out_status.res
	-diff -wc out17.out out17.res || echo $$? >> out_status.res
	-diff -wc out18.out out18.res || echo $$? >> out_status.res
	-diff -wc out19.out out19.res || echo $$? >> out_status.res
	-diff -wc out20.out out20.res || echo $$? >> out_status.res
	-diff -wc out21.out out21.res || echo $$? >> out_status.res
	test ! -e out_status.res

clean:
	rm -f *.res *~

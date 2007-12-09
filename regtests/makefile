
.SILENT:

all: run_tests check_results

run_tests:
	-../style_checker -H error1.txt demo.adb > out1.res 2>&1
	-../style_checker -HLSBCT error1.txt demo.adb > out2.res 2>&1
	-../style_checker -lang Ada -HS error1.txt demo.adb > out3.res 2>&1
	-../style_checker -HL error1.txt azerty.adb > out4.res 2>&1
	-../style_checker -HBCELT -lang Ada main.adb > out5.res 2>&1
	-../style_checker -HBCELT -lang Ada -cy main.adb > out6.res 2>&1
	-../style_checker -HBCELT -lang Ada -cp main.adb > out7.res 2>&1
	-../style_checker -H endings.txt > out8.res 2>&1
	-../style_checker -H noendings.txt > out9.res 2>&1
	-../style_checker hello.e > out10.res 2>&1
	-../style_checker -ign e hello.e >> out10.res 2>&1
	-../style_checker -m 2 hello.e > out11.res 2>&1
	-../style_checker header1.adb header2.adb header3.adb > out12.res 2>&1
	-../style_checker -S -cY -H \
		-cf'Copyright \(C\) 2\d\d\d-2\d\d\d, (Pascal Obry|Qwerty)' \
		copyright.ads > out13.res 2>&1

check_results:
	echo "### regressions follows"
	-diff -wc out1.out out1.res
	-diff -wc out2.out out2.res
	-diff -wc out3.out out3.res
	-diff -wc out4.out out4.res
	-diff -wc out5.out out5.res
	-diff -wc out5.out out6.res
	-diff -wc out5.out out7.res
	-diff -wc out8.out out8.res
	-diff -wc out9.out out9.res
	-diff -wc out10.out out10.res
	-diff -wc out11.out out11.res
	-diff -wc out12.out out12.res
	-diff -wc out13.out out13.res

clean:
	rm -f *.res *~

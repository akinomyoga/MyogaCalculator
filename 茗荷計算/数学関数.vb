Module math
#Region "êÆêî"
    Public Function LCM(ByVal num1, ByVal num2) As Double 'ç≈ëÂåˆñÒêî
        Dim num, a
        If num1 = 0 Then LCM = num2 : Exit Function
        If num2 = 0 Then LCM = num1 : Exit Function
        If num1 < num2 Then num = num1 : num1 = num2 : num2 = num
        LCM = 1
        For a = 2 To num2
            While num1 Mod a = 0 And num2 Mod a = 0
                LCM *= a
                num1 /= a
                num2 /= a
            End While
        Next a
    End Function
    Public Function GCM(ByVal num1, ByVal num2) As Double 'ç≈è¨åˆî{êî
        GCM = num1 * num2 / LCM(num1, num2)
    End Function
    Public Function ceil(ByVal number As Double, Optional ByVal unit As Double = 1) As Double 'ìVà‰
        ceil = System.Math.Ceiling(number / unit) * unit
    End Function
    Public Function floor(ByVal number As Double, Optional ByVal unit As Double = 1) As Double 'è∞
        floor = System.Math.Floor(number / unit) * unit
    End Function
    Public Function ceiflo(ByVal number As Double, Optional ByVal unit As Double = 1, Optional ByVal separate As Byte = 5) As Double 'éÃì¸
        If number < 0 Then
            ceiflo = -Microsoft.VisualBasic.Int(-number / unit + 1 - separate / 10) * unit
        Else
            ceiflo = Microsoft.VisualBasic.Int(number / unit + 1 - separate / 10) * unit
        End If
    End Function
    Public Function int(ByVal number As Double, Optional ByVal unit As Double = 1) As Double '0Ç…å¸Ç©Ç¡ÇƒêÿÇËéÃÇƒ
        int = Microsoft.VisualBasic.Int(number / unit) * unit
    End Function
    Public Function fix(ByVal number As Double, Optional ByVal unit As Double = 1) As Double '0Ç…å¸Ç©Ç¡ÇƒêÿÇËè„Ç∞
        fix = Microsoft.VisualBasic.Fix(number / unit) * unit
    End Function
    Public Function round(ByVal number As Double, Optional ByVal unit As Double = 1, Optional ByVal separate As Byte = 5) As Double 'éléÃå‹ì¸
        round = Microsoft.VisualBasic.Int(number / unit + System.Math.Sign(number) * (1 - separate / 10)) * unit
    End Function
#End Region
#Region "èÊ"
    Public Const fctl0 As Double = 1
    Public Const fctl1 As Double = 1
    Public Const fctl2 As Double = 2
    Public Const fctl3 As Double = 6
    Public Const fctl4 As Double = 24
    Public Const fctl5 As Double = 120
    Public Const fctl6 As Double = 720
    Public Const fctl7 As Double = 5040
    Public Const fctl8 As Double = 40320
    Public Const fctl9 As Double = 362880
    Public Const fctl10 As Double = 3628800
    Public Const fctl11 As Double = 39916800
    Public Const fctl12 As Double = 479001600
    Public Const fctl13 As Double = 6227020800
    Public Const fctl14 As Double = 87178291200
    Public Const fctl15 As Double = 1307674368000
    Public Const fctl16 As Double = 20922789888000
    Public Const fctl17 As Double = 355687428096000
    Public Const fctl18 As Double = 6402373705728000
    Public Const fctl19 As Double = 121645100408832000
    Public Const fctl20 As Double = 2432902008176640000
    Public Function XnRise(ByVal x As Double, ByVal n As Integer) As Double 'x^nÅP
        Dim a
        XnRise = x
        If n > 1 Then
            For a = x + 1 To x + n - 1 Step 1
                XnRise = XnRise * a
            Next a
        ElseIf n = 0 Then
            XnRise = 1
        ElseIf n < 0 Then
            XnRise = XnFall(x, -n)
        End If
    End Function
    Public Function XnFall(ByVal x As Double, ByVal n As Double) As Double 'x^n_
        If Not Microsoft.VisualBasic.Int(n) = n Then Return É°(x + 1) / É°(x + 1 - n)
        If n = 0 Then Return 1
        If n < 0 Then Return XnRise(x, -n)
        Dim a
        XnFall = x
        If n > 1 Then
            For a = x - 1 To x - n + 1 Step -1
                XnFall = XnFall * a
            Next a
        Else
        End If
    End Function
    Public Function fctl(ByVal n As Double) As Double '!
        If n < 0 Then Return É°(n + 1)
        If Not Microsoft.VisualBasic.Int(n) = n Then Return É°(n + 1)
        Dim a
        fctl = 1
        If n <> 0 Then Exit Function
        Do While Not n < 1
            fctl *= n
            n -= 1
        Loop
    End Function
    Public Function fcfc(ByVal n As Double) '!!
        If n < 0 Then Return 0 'ñ¢íËã`
        If Not Microsoft.VisualBasic.Int(n) = n Then Return 0 'ñ¢íËã`
        Dim a
        fcfc = 1
        If n = 0 Then Exit Function
        Do While Not n < 1
            fcfc *= n
            n -= 2
        Loop
    End Function

    Public Function mHn(ByVal m As Integer, ByVal n As Integer) As Double 'èdï°ëgçá
        mHn = mCn(m + n - 1, n)
    End Function
    Public Function mPIn(ByVal m As Double, ByVal n As Double) As Double 'èdï°èáóÒ
        m = System.Math.Abs(m)
        n = System.Math.Abs(n)
        mPIn = m ^ n
    End Function
    Public Function mCn(ByVal m As Double, ByVal n As Double) As Double 'ëgçá
        If m >= n Then
            mCn = XnFall(m, n) / fctl(n)
        Else
            mCn = XnFall(n, m) / fctl(m)
            MsgBox("mCnä÷êîÇÕmÇÊÇËnÇÃï˚Ç™ëÂÇ´Ç¢ÇΩÇﬂÅAmÇ∆nÇãtì]ÇµÇƒåvéZÇµÇ‹Ç∑ÅB")
        End If
    End Function
    Public Function mPn(ByVal m As Double, ByVal n As Double) As Double 'èáóÒ
        If m >= n Then
            mPn = XnFall((m), n)
        Else
            mPn = XnFall((n), m)
            MsgBox("mPnä÷êîÇÕmÇÊÇËnÇÃï˚Ç™ëÂÇ´Ç¢ÇΩÇﬂÅAmÇ∆nÇãtì]ÇµÇƒåvéZÇµÇ‹Ç∑ÅB")
        End If
    End Function
    Public Class mP
        Public Function circle(ByVal m As Double) As Double 'â~èáóÒ
            circle = fctl(m - 1)
        End Function
        Public Function juzu(ByVal m As Double) As Double 'êîéÏèáóÒ
            juzu = fctl(m - 1)
            If m > 1 Then juzu /= 2
        End Function
    End Class
    Public Function mE1n(ByVal m As Integer, ByVal n As Integer) 'Eulerêî
        If n = 0 Then
            mE1n = 0
        ElseIf n < 0 Or n >= m Then
            mE1n = 1
        Else
            mE1n = (n + 1) * mE1n(m - 1, n) + (m - n) * mE1n(m - 1, n - 1)
        End If
    End Function
    Public Function mE2n(ByVal m As Integer, ByVal n As Integer) As Double 'Eulerêî
        n = n - 1
        If n = 0 Then
            mE2n = 0
        ElseIf n < 0 Or n >= m Then
            mE2n = 1
        Else
            mE2n = (n + 1) * mE2n(m - 1, n) + (m - n) * mE2n(m - 1, n - 1)
        End If
    End Function
    Public Function mS1n(ByVal m As Integer, ByVal n As Integer) As Integer 'Stirlingêî
        If n < 1 Or n > m Then
            mS1n = 0
        ElseIf n = m Then
            mS1n = 1
        Else
            mS1n = (m - 1) * mS1n(m - 1, n) + mS1n(m - 1, n - 1)
        End If
    End Function
    Public Function mS2n(ByVal m As Integer, ByVal n As Integer) As Double 'Stirlingêî
        If n < 1 Or n > m Then
            mS2n = 0
        ElseIf n = 1 Or n = m Then
            mS2n = 1
        Else
            mS2n = n * mS2n(m - 1, n) + mS2n(m - 1, n - 1)
        End If
    End Function

#End Region
#Region "éwêîä÷êî ëŒêîä÷êî"
    Public Const e = 2.7182818284590451
    Public Function exp(ByVal number As Double) As Double
        exp = System.Math.Exp(number)
    End Function
    Public Function pow(ByVal number As Double, ByVal exponent As Double) As Double
        If number < 0 And (1 / exponent) Mod 2 = 0 Then pow = 0 : Exit Function
        pow = System.Math.Pow(number, exponent)
    End Function
    Public Function logbi(ByVal number As Double) As Double
        logbi = System.Math.Log(number) / System.Math.Log(2)
    End Function
    Public Function ln(ByVal number As Double) As Double
        ln = System.Math.Log(number)
    End Function
    Public Function lognib(ByVal number As Double) As Double
        lognib = System.Math.Log(number) / System.Math.Log(4)
    End Function
    Public Function logo(ByVal number As Double) As Double
        logo = System.Math.Log(number) / System.Math.Log(8)
    End Function
    Public Function log(ByVal number As Double) As Double
        log = System.Math.Log10(number)
    End Function
    Public Function logh(ByVal number As Double) As Double
        logh = System.Math.Log(number) / System.Math.Log(16)
    End Function
    Public Function logbyte(ByVal number As Double) As Double
        logbyte = System.Math.Log(number) / System.Math.Log(256)
    End Function
    Public Function loga(ByVal a As Double, ByVal number As Double) As Double
        loga = System.Math.Log10(number) / System.Math.Log10(a)
    End Function
#End Region
    Public Const euler As Double = 0.577215664901533
    Public Const pi As Double = 3.1415926535897931
    Public Const hlfpi As Double = 1.5707963267948966
    Public Const qtrpi As Double = 0.78539816339744828


#Region "ë»â~ä÷êî"
    'àÍî ëÊìÒéÌë»â~ä÷êî
    Public Function el2(ByVal x As Double, ByVal kc As Double, ByVal a As Double, ByVal b As Double) As Double
        If kc = 0 Then el2 = 0 : Exit Function
        If x = 0 Then el2 = 0 : Exit Function
        Dim c As Double, d As Double, p As Double, eye As Double, y As Double, z As Double, e As Double, g As Double
        Dim f As Double = 0
        Dim l As Double = 0
        Dim em As Double = 1
        c = x * x
        d = 1 + c
        p = System.Math.Sqrt((1 + c * kc * kc) / d)
        d = x / d
        c = d / 2 * p
        eye = a
        z = a - b
        a = (a + b) / 2
        y = System.Math.Abs(1 / x)
        kc = System.Math.Abs(kc)
        Do
            b += eye * kc
            e = em * kc
            g = e / p
            d += f * g
            f = c
            eye = a
            p += g
            c = (d / p + c) / 2
            g = em
            em += kc
            a = (b / em + a) / 2
            y -= e / y
            If y = 0 Then y = System.Math.Sqrt(e) * 0.0000000000000001
            If System.Math.Abs(g - kc) <= g * 0.0000000000000001 Then Exit Do
            kc = 2 * System.Math.Sqrt(e)
            l *= 2
            If y < 0 Then l += 1
        Loop
        If y < 0 Then l += 1
        e = (System.Math.Atan(em / y) + l * pi) / em
        If x < 0 Then e = -e
        el2 = c * z
    End Function
    'àÍî äÆëSë»â~êœï™
    Public Function cel(ByVal kc As Double, ByVal p As Double, ByVal a As Double, ByVal b As Double) As Double
        Dim em As Double = 1
        Dim e As Double, f As Double, g As Double, q As Double
        kc = System.Math.Abs(kc)
        e = kc
        If p > 0 Then
            p = System.Math.Sqrt(p)
            b /= p
        Else
            f = kc * kc
            q = 1 - f
            g = 1 - p
            q *= b - a * p
            p = System.Math.Sqrt((f - p) / g)
            a = (a - b) / g
            b = a * p - q / g / g / p
        End If
        Do
            f = a
            a += b / p
            g = e / p
            b += f * g
            b *= 2
            p += g
            g = em
            em += q * c
            If System.Math.Abs(g - kc) <= g * 0.0000000000000001 Then Exit Do
            kc = 2 * System.Math.Sqrt(e)
            e = kc * em
        Loop
        cel = hlfpi / em * (b + a * em) / (em + p)
    End Function
    'ëÊàÍéÌLegendreêœï™
    Public Function F(ByVal phi As Double, ByVal k As Double) As Double
        If k * k > 1 Then F = 0 : Exit Function
        F = el2(System.Math.Tan(phi), System.Math.Sqrt(1 - k * k), 1, 1)
    End Function
    Public Function F2(ByVal phi As Double, ByVal alpha As Double) As Double
        F2 = el2(System.Math.Tan(phi), System.Math.Cos(alpha), 1, 1)
    End Function

#End Region

#Region "ãÊä‘"
    Public Function inSection(ByVal ïœêî As Double, ByVal ãÊä‘ As String) As Boolean
        ãÊä‘ = Trim(ãÊä‘)
        Dim l As Integer = Len(ãÊä‘)
        Dim mineq As Boolean = False : If Left(ãÊä‘, 1) = "[" Then mineq = True
        Dim maxeq As Boolean = False : If Right(ãÊä‘, 1) = "]" Then maxeq = True
        l -= 2 : ãÊä‘ = Mid(ãÊä‘, 2, l - 2)
        Dim a As Integer, b As Integer
        For a = 1 To l
            If Mid(ãÊä‘, a, 1) = "," Then b = a
        Next
        If mineq Then
            inSection = CDbl(Mid(ãÊä‘, 1, b - 1)) <= ïœêî
        Else
            inSection = CDbl(Mid(ãÊä‘, 1, b - 1)) < ïœêî
        End If
        If maxeq Then
            inSection = inSection And CDbl(Mid(ãÊä‘, b + 1)) >= ïœêî
        Else
            inSection = inSection And CDbl(Mid(ãÊä‘, b + 1)) > ïœêî
        End If
    End Function
    Public Function MinMax(ByVal min As Double, ByVal num As Double, ByVal max As Double) As Boolean
        MinMax = min <= num And num <= max
    End Function
    Public Function MinSup(ByVal min As Double, ByVal num As Double, ByVal sup As Double) As Boolean
        MinSup = min <= num And num < sup
    End Function
    Public Function InfMax(ByVal inf As Double, ByVal num As Double, ByVal max As Double) As Boolean
        InfMax = inf < num And num <= max
    End Function
    Public Function InfSup(ByVal inf As Double, ByVal num As Double, ByVal sup As Double) As Boolean
        InfSup = inf < num And num < sup
    End Function
#End Region

    Public Function gd(ByVal x As Double) As Double 'Gudermann
        gd = System.Math.Atan(System.Math.Sinh(x))
    End Function
    Public Function arcgd(ByVal x As Double) As Double 'ãtGudermann Lamberg
        arcgd = System.Math.Log(System.Math.Tan(x / 2 + qtrpi))
    End Function
    Public Function W(ByVal x As Double) As Double 'èdÇ›ä÷êî
        If 1 - x * x < 0 Then W = 0 : Exit Function
        W = 1 / System.Math.Sqrt(1 - x * x)
    End Function
End Module

Module str
    Public Function DblStr(ByVal number As Double) As String
        Dim a
        DblStr = number
        a = Split(DblStr, "e", 2)
        If UBound(a) = 0 Then Exit Function 'e=0ÇÃéû
        a(0) = Left(a(0), 1) & Mid(a(0), 3)
        If a(1) < 0 Then DblStr = "0." & StringProd("0", -1 - a(1)) & a(0) : Exit Function 'e<0ÇÃéû
        If Len(a(0)) <= 1 + a(1) Then DblStr = a(0) & StringProd("0", 1 + a(1) - Len(a(0))) : Exit Function '0<e<åÖêî
        DblStr = Left(a(0), 1 + a(1)) & "." & Mid(a(0), 2 + a(1)) 'åÖêî<eÇÃéû
    End Function
    Public Function StringProd(ByVal a As String, ByVal n As Integer)
        Dim number
        StringProd = ""
        If number <= 0 Then Exit Function
        For n = 1 To number
            StringProd = StringProd & a
        Next n
    End Function
    Public Function StrReIns(ByVal str1 As String, ByVal target As Integer, ByVal str2 As String) As String
        '[ï∂éöóÒ1] ÇÃ [n] î‘ñ⁄ÇÃ1ï∂éöÇ [ï∂éöóÒ2] Ç…íuÇ´ä∑Ç¶Ç‹Ç∑
        StrReIns = Left(str1, target - 1) & str2 & Mid(str1, target + 1)
    End Function
    Public Function StrReWri(ByVal str As String, ByVal start As Integer, ByVal str2 As String) As String
        '[ï∂éöóÒ1] ÇÃ [n] î‘ñ⁄ÇÃï∂éöÇ©ÇÁ [ï∂éöóÒ2] Ç…íuÇ´ä∑Ç¶Ç‹Ç∑
        StrReWri = Left(str, start - 1) & str2 & Mid(str, start + Len(str2))
    End Function
    Public Function StrIns(ByVal str As String, ByVal portion As Integer, ByVal str2 As String) As String
        StrIns = Left(str, portion) & str2 & Mid(str, portion + 1)
    End Function
    Public Sub Change2(ByRef expression1, ByRef expression2)
        Dim expression0 = expression1
        expression1 = expression2
        expression2 = expression0
    End Sub
    Public Sub Change3RtL(ByRef a, ByRef b, ByRef c)
        Dim d = a
        a = b
        b = c
        c = d
    End Sub
    Public Sub change3LtR(ByRef a, ByRef b, ByRef c)
        Dim d = a
        a = c
        c = b
        b = d
    End Sub
End Module

Public Module Tri
#Region "äpìx"
    Public Function DegRad(ByVal degree As Double) As Double
        DegRad = degree * System.Math.PI / 180
    End Function
    Public Function RadDeg(ByVal radian As Double) As Double
        RadDeg = radian * 180 / System.Math.PI
    End Function
#End Region
#Region "éOäpä÷êî"
    Public Function sin(ByVal number As Double) As Double
        sin = System.Math.Sin(number)
    End Function
    Public Function cos(ByVal number As Double) As Double
        cos = System.Math.Cos(number)
    End Function
    Public Function tan(ByVal number As Double) As Double
        tan = System.Math.Tan(number)
    End Function
    Public Function cot(ByVal number As Double) As Double
        cot = 1 / System.Math.Tan(number)
    End Function
    Public Function sec(ByVal number As Double) As Double
        sec = 1 / System.Math.Cos(number)
    End Function
    Public Function cosec(ByVal number As Double) As Double
        cosec = 1 / System.Math.Sin(number)
    End Function
    Public Function vers(ByVal number As Double) As Double
        vers = 1 - System.Math.Cos(number)
    End Function
    Public Function covers(ByVal number As Double) As Double
        covers = 1 - System.Math.Sin(number)
    End Function
    Public Function hav(ByVal number As Double) As Double
        hav = (1 - System.Math.Cos(number)) / 2
    End Function
    Public Function cohav(ByVal number As Double) As Double
        cohav = (1 - System.Math.Sin(number)) / 2
    End Function
    Public Function exsec(ByVal number As Double) As Double
        exsec = 1 / System.Math.Cos(number) - 1
    End Function
    Public Function excosec(ByVal number As Double) As Double
        excosec = 1 / System.Math.Sin(number) - 1
    End Function
#End Region
#Region "ëoã»ê¸ä÷êî"
    Public Function sinh(ByVal number As Double) As Double
        sinh = System.Math.Sinh(number)
    End Function
    Public Function cosh(ByVal number As Double) As Double
        cosh = System.Math.Cosh(number)
    End Function
    Public Function tanh(ByVal number As Double) As Double
        tanh = System.Math.Tanh(number)
    End Function
    Public Function sech(ByVal number As Double) As Double
        sech = 1 / System.Math.Cosh(number)
    End Function
    Public Function coth(ByVal number As Double) As Double
        coth = 1 / System.Math.Tanh(number)
    End Function
    Public Function cosech(ByVal number As Double) As Double
        cosech = 1 / System.Math.Sinh(number)
    End Function
    Public Function versh(ByVal number As Double) As Double
        versh = 1 - System.Math.Cosh(number)
    End Function
    Public Function coversh(ByVal number As Double) As Double
        coversh = 1 - System.Math.Sinh(number)
    End Function
    Public Function havh(ByVal number As Double) As Double
        havh = (1 - System.Math.Cosh(number)) / 2
    End Function
    Public Function cohavh(ByVal number As Double) As Double
        cohavh = (1 - System.Math.Sinh(number)) / 2
    End Function
    Public Function exsech(ByVal number As Double) As Double
        exsech = 1 / System.Math.Cosh(number) - 1
    End Function
    Public Function excosech(ByVal number As Double) As Double
        excosech = 1 / System.Math.Sinh(number) - 1
    End Function
#End Region
    Public Class Arc
#Region "ãtéOäpä÷êî"
        Public Shared Function sin(ByVal number As Double) As Double
            sin = System.Math.Asin(number)
        End Function
        Public Shared Function cos(ByVal number As Double) As Double
            cos = System.Math.Acos(number)
        End Function
        Public Shared Function tan(ByVal number As Double) As Double
            tan = System.Math.Atan(number)
        End Function
        Public Shared Function cot(ByVal number As Double) As Double
            cot = System.Math.Atan(number) + hlfpi
        End Function
        Public Shared Function sec(ByVal number As Double) As Double
            sec = hlfpi - System.Math.Atan(System.Math.Sign(number) / System.Math.Sqrt(number * number - 1))
        End Function
        Public Shared Function cosec(ByVal number As Double) As Double
            cosec = System.Math.Atan(System.Math.Sign(number) / System.Math.Sqrt(number * number - 1))
        End Function
        Public Shared Function vers(ByVal number As Double) As Double
            vers = System.Math.Acos(1 - number)
        End Function
        Public Shared Function covers(ByVal number As Double) As Double
            covers = System.Math.Asin(1 - number)
        End Function
        Public Shared Function hav(ByVal number As Double) As Double
            hav = System.Math.Acos(1 - 2 * number)
        End Function
        Public Shared Function cohav(ByVal number As Double) As Double
            cohav = System.Math.Asin(1 - 2 * number)
        End Function
        Public Shared Function exsec(ByVal number As Double) As Double
            exsec = hlfpi - System.Math.Atan(System.Math.Sign(number + 1) / System.Math.Sqrt(number * (number + 2)))
        End Function
        Public Shared Function excosec(ByVal number As Double) As Double
            excosec = System.Math.Atan(System.Math.Sign(number + 1) / System.Math.Sqrt(number * (number + 2)))
        End Function
#End Region
#Region "ãtëoã»ê¸ä÷êî"
        Public Shared Function sinh(ByVal number As Double) As Double
            sinh = System.Math.Log(number + System.Math.Sqrt(number * number + 1))
        End Function
        Public Shared Function cosh(ByVal number As Double) As Double
            cosh = System.Math.Log(number + System.Math.Sqrt(number * number - 1))
        End Function
        Public Shared Function tanh(ByVal number As Double) As Double
            tanh = System.Math.Log((1 + number) / (1 - number)) / 2
        End Function
        Public Shared Function coth(ByVal number As Double) As Double
            coth = System.Math.Log((number + 1) / (number - 1)) / 2
        End Function
        Public Shared Function sech(ByVal number As Double) As Double
            sech = System.Math.Log((System.Math.Sqrt(-number * number + 1) + 1) / number)
        End Function
        Public Shared Function cosech(ByVal number As Double) As Double
            cosech = System.Math.Log((System.Math.Sign(number) * System.Math.Sqrt(number * number + 1) + 1) / number)
        End Function
        Public Shared Function versh(ByVal number As Double) As Double
            versh = System.Math.Log(1 - number + System.Math.Sqrt(number * (number - 2)))
        End Function
        Public Shared Function coversh(ByVal number As Double) As Double
            coversh = System.Math.Log(1 - number + System.Math.Sqrt(number * (number - 2) + 2))
        End Function
        Public Shared Function havh(ByVal number As Double) As Double
            havh = System.Math.Log(2 - 2 * number + 2 * System.Math.Sqrt(number * (number - 1)))
        End Function
        Public Shared Function cohavh(ByVal number As Double) As Double
            cohavh = System.Math.Log(2 - 2 * number + System.Math.Sqrt(4 * number * (number - 1) + 2))
        End Function
        Public Shared Function exsech(ByVal number As Double) As Double
            exsech = System.Math.Log((System.Math.Sqrt(-number * (number + 2)) + 1) / (number + 1))
        End Function
        Public Shared Function excosech(ByVal number As Double) As Double
            excosech = System.Math.Log((System.Math.Sign(number + 1) * System.Math.Sqrt(number * (number + 2) + 2) + 1) / number)
        End Function
#End Region
    End Class
End Module

Module complex
    Structure complex
        Public Re As Double
        Public Im As Double
#Region "Complex Functions"
        Public Overloads Function plus(ByVal number As complex) As complex
            plus.Re = Re + number.Re
            plus.Im = Im + number.Im
        End Function
        Public Overloads Function plus(ByVal number As Double) As complex
            plus.Re = Re + number
            plus.Im = Im
        End Function

        Public Overloads Function minus() As complex
            minus.Re = -Re
            minus.Im = -Im
        End Function
        Public Overloads Function minus(ByVal number As complex) As complex
            minus.Re = Re - number.Re
            minus.Im = Im - number.Im
        End Function
        Public Overloads Function minus(ByVal number As Double) As complex
            minus.Re = Re - number
            minus.Im = Im
        End Function

        Public Function recip() As complex 'reciprocal complex number:ãtêî
            Dim div As Double
            div = Re ^ 2 + Im ^ 2
            recip.Re = Re / div
            recip.Im = -Im / div
        End Function

        Public Overloads Function div(ByVal number As complex) As complex
            div = DblCmp(Re, Im).multi(number.conj).div(number.Re * number.Re + number.Im * number.Im)
        End Function
        Public Overloads Function div(ByVal number As Double) As complex
            div.Re = Re / number
            div.Im = Im / number
        End Function

        Public Overloads Function multi(ByVal number As complex) As complex
            multi.Re = Re * number.Re - Im * number.Im
            multi.Im = Re * number.Im + Im * number.Re
        End Function
        Public Overloads Function multi(ByVal number As Double) As complex
            multi.Re = Re * number
            multi.Im = Im * number
        End Function

        Public Function conj() As complex 'conjugate complex number:ã§ñï°ëfêî
            conj.Re = Re
            conj.Im = -Im
        End Function
        Public Function abs() As complex 'ê‚ëŒíl|z|
            abs.Re = System.Math.Sqrt(Re * Re + Im * Im)
            abs.Im = 0
        End Function

        Public Overloads Function power(ByVal number As complex, ByVal exponent As complex) As complex 'ó›èÊ
            Dim num = DblCmp(number.absD, number.arg).multi(exponent)
            power = DblCmp(System.Math.Cos(num.im), System.Math.Sin(num.im)).multi(System.Math.Exp(num.re))
        End Function
        Public Overloads Function power(ByVal exponent As complex) As complex
            Dim num = DblCmp(Me.absD, Me.arg).multi(exponent)
            power = DblCmp(System.Math.Cos(num.im), System.Math.Sin(num.im)).multi(System.Math.Exp(num.re))
        End Function
        Public Overloads Function power(ByVal exponent As Double) As complex
            Dim num = DblCmp(Me.absD, Me.arg).multi(exponent)
            power = DblCmp(System.Math.Cos(num.im), System.Math.Sin(num.im)).multi(System.Math.Exp(num.re))
        End Function

        Public Function arg() As Double 'ïŒäp:arg z
            If Re = 0 Then arg = math.hlfpi
            arg = System.Math.Atan(Im / Re) + (Im < 0) * (math.pi)
        End Function
        Public Function absD() As Double 'ê‚ëŒíl|z|
            absD = System.Math.Sqrt(Re ^ 2 + Im ^ 2)
        End Function
        Public Function str() As String
            If Re = 0 Then
                If Im = 0 Then str = "0" Else str = DblStr(Im) & "i"
            ElseIf Im = 0 Then
                str = DblStr(Re)
            Else
                str = DblStr(Re) & "+" & DblStr(Im) & "i"
            End If
        End Function
#End Region
    End Structure
    Public i As complex = DblCmp(0, 1)

#Region "élë•"
    Public Overloads Function plus(ByVal complex0 As complex, ByVal complex1 As complex) As complex
        plus = complex0.plus(complex1)
    End Function
    Public Overloads Function plus(ByVal complex0 As complex, ByVal complex1 As complex, ByVal complex2 As complex) As complex
        plus.Re = complex0.Re + complex1.Re + complex2.Re
        plus.Im = complex0.Im + complex1.Im + complex2.Im
    End Function
    Public Overloads Function plus(ByVal complex0 As complex, ByVal complex1 As complex, ByVal complex2 As complex, ByVal complex3 As complex) As complex
        plus.Re = complex0.Re + complex1.Re + complex2.Re + complex3.Re
        plus.Im = complex0.Im + complex1.Im + complex2.Im + complex3.Im
    End Function
    Public Overloads Function multi(ByVal complex0 As complex, ByVal complex1 As complex) As complex
        multi.Re = complex0.Re * complex1.Re - complex0.Im * complex1.Im
        multi.Im = complex0.Re * complex1.Im + complex0.Im * complex1.Re
    End Function
    Public Overloads Function multi(ByVal complex0 As complex, ByVal complex1 As complex, ByVal complex2 As complex) As complex
        Dim cmp As complex
        cmp.Re = complex0.Re * complex1.Re - complex0.Im * complex1.Im
        cmp.Im = complex0.Re * complex1.Im + complex0.Im * complex1.Re
        multi.Re = cmp.Re * complex2.Re - cmp.Im * complex2.Im
        multi.Im = cmp.Re * complex2.Im + cmp.Im * complex2.Re
    End Function
    Public Overloads Function multi(ByVal complex0 As complex, ByVal complex1 As complex, ByVal complex2 As complex, ByVal complex3 As complex) As complex
        Dim cmp0 As complex, cmp1 As complex
        cmp0.Re = complex0.Re * complex1.Re - complex0.Im * complex1.Im
        cmp0.Im = complex0.Re * complex1.Im + complex0.Im * complex1.Re
        cmp1.Re = complex2.Re * complex3.Re - complex2.Im * complex3.Im
        cmp1.Im = complex2.Re * complex3.Im + complex2.Im * complex3.Re
        multi.Re = cmp0.Re * cmp1.Re - cmp0.Im * cmp1.Im
        multi.Im = cmp0.Re * cmp1.Im + cmp0.Im * cmp1.Re
    End Function
#End Region
#Region "äeå`éÆÇÃïœä∑"
    Public Overloads Function DblCmp(ByVal Real As Double, ByVal Imaginary As Double) As complex
        DblCmp.Re = Real
        DblCmp.Im = Imaginary
    End Function
    Public Overloads Function DblCmp(ByVal Real As Double) As complex
        DblCmp.Re = Real
        DblCmp.Im = 0
    End Function
    Public Function RCmp(ByVal radius As Double, ByVal arg As Double) As complex 'ã…å`éÆÇ©ÇÁÇÃïœä∑
        RCmp.Re = System.Math.Cos(arg)
        RCmp.Im = System.Math.Sin(arg)
        RCmp = RCmp.multi(radius)
    End Function
#End Region
#Region "éOäp"
    Public Function sin(ByVal number As complex) As complex
        sin.Re = System.Math.Sin(number.Re) * System.Math.Cosh(number.Im)
        sin.Im = System.Math.Cos(number.Re) * System.Math.Sinh(number.Im)
    End Function
    Public Function cos(ByVal number As complex) As complex
        cos.Re = System.Math.Cos(number.Re) * System.Math.Cosh(number.Im)
        cos.Im = -System.Math.Sin(number.Re) * System.Math.Sinh(number.Im)
    End Function
    Public Function tan(ByVal number As complex) As complex
        tan.Re = System.Math.Sin(number.Re * 2)
        tan.Im = System.Math.Sinh(number.Im * 2)
        tan = tan.div(System.Math.Cos(number.Re * 2) + System.Math.Cosh(number.Im * 2))
    End Function
    Public Function cot(ByVal number As complex) As complex
        cot.Re = System.Math.Sin(number.Re * 2)
        cot.Im = -System.Math.Sinh(number.Im * 2)
        cot = cot.div(System.Math.Cosh(number.Im * 2) - System.Math.Cos(number.Re * 2))
    End Function
    Public Function sec(ByVal number As complex) As complex
        sec.Re = System.Math.Sin(number.Re) * System.Math.Cosh(number.Im)
        sec.Im = System.Math.Cos(number.Re) * System.Math.Sinh(number.Im)
        sec = sec.div(System.Math.Cosh(number.Im) ^ 2 - System.Math.Sin(number.Re) ^ 2)
    End Function
    Public Function cosec(ByVal number As complex) As complex
        cosec.Re = System.Math.Cos(number.Re) * System.Math.Cosh(number.Im)
        cosec.Im = System.Math.Sin(number.Re) * System.Math.Sinh(number.Im)
        cosec = cosec.div(System.Math.Cosh(number.Im) ^ 2 - System.Math.Sin(number.Re) ^ 2)
    End Function
#End Region
#Region "ëoã»ê¸"
    Public Function sinh(ByVal number As complex) As complex
        sinh.Re = System.Math.Sinh(number.Re) * System.Math.Cos(number.Im)
        sinh.Im = System.Math.Cosh(number.Re) * System.Math.Sin(number.Im)
    End Function
    Public Function cosh(ByVal number As complex) As complex
        cosh.Re = System.Math.Cosh(number.Re) * System.Math.Cos(number.Im)
        cosh.Im = System.Math.Sinh(number.Re) * System.Math.Sin(number.Im)
    End Function
    Public Function tanh(ByVal number As complex) As complex
        tanh.Re = System.Math.Sinh(2 * number.Re)
        tanh.Im = System.Math.Sin(2 * number.Im)
        tanh = tanh.div(System.Math.Cosh(2 * number.Re) + System.Math.Cos(2 * number.Im))
    End Function
    Public Function coth(ByVal number As complex) As complex
        coth.Re = System.Math.Sinh(2 * number.Re)
        coth.Im = -System.Math.Sin(2 * number.Im)
        coth = coth.div(System.Math.Cosh(2 * number.Re) - System.Math.Cos(2 * number.Im))
    End Function
    Public Function sech(ByVal number As complex) As complex
        sech = cosh(number).recip
    End Function
    Public Function cosech(ByVal number As complex) As complex
        cosech = sinh(number).recip
    End Function
#End Region
#Region "ëŒêî"
    Public Function ln(ByVal number As complex) As complex
        ln.Re = System.Math.Log(number.absD)
        ln.Im = number.arg
    End Function
#End Region
    Public Class Arc
#Region "ãtéOäp"
        Public Shared Function sin(ByVal number As complex) As complex
            sin = tan(number.div(number.multi(number).minus.plus(1).power(0.5)))'t^2=s^2/c^2=s^2/1-s^2åÃt=x/Å„(1-x^2)
        End Function
        Public Shared Function cos(ByVal number As complex) As complex
            cos = tan(number.minus.div(number.multi(number).minus.plus(1).power(0.5))).plus(hlfpi)
        End Function
        Public Shared Function tan(ByVal number As complex) As complex
            number = i.multi(number) 'ix
            number = number.plus(1).div(number.minus.plus(1)) '(1+ix)/(1-ix)
            tan = i.multi(-1 / 2).multi(ln(number))
        End Function
        Public Shared Function cot(ByVal number As complex) As complex
            cot = tan(number).minus.plus(hlfpi)
        End Function
        Public Shared Function sec(ByVal number As complex) As complex
            number = number.multi(number).minus(1).power(-0.5) 'X=1 / Å„(x^2-1)
            sec = tan(number).minus.plus(hlfpi) 'arc sec x = - arc tan X + ÉŒ / 2
        End Function
        Public Shared Function cosec(ByVal number As complex) As complex
            number = number.multi(number).minus(1).power(-0.5) 'X=1 / Å„(x^2-1)
            cosec = tan(number)
        End Function
#End Region
#Region "ãtëoã»ê¸"
        Public Shared Function sinh(ByVal number As complex) As complex
            sinh = number.multi(number).plus(1).power(0.5) 'a=Å„(x^2+1)
            sinh = ln(number.plus(sinh)) 'arc sinh x = ln(x + a)
        End Function
        Public Shared Function cosh(ByVal number As complex) As complex
            cosh = number.multi(number).minus(1).power(0.5) 'a=Å„(x^2-1)
            cosh = ln(number.plus(cosh)) 'arc cosh x== ln(x + a)
        End Function
        Public Shared Function tanh(ByVal number As complex) As complex
            tanh = number.plus(1).div(number.minus(1).minus) 'a=(1+x)/(1-x)
            tanh = ln(tanh).div(2) 'arc tanh x = ln(a)/2
        End Function
        Public Shared Function coth(ByVal number As complex) As complex
            coth = number.plus(1).div(number.minus(1)) 'a=(x+1)/(x-1)
            coth = ln(coth).div(2) 'arc coth x = ln(a)/2
        End Function
        Public Shared Function sech(ByVal number As complex) As complex
            sech = number.multi(number).minus.plus(1).power(0.5) 'a=Å„(1-x^2)
            sech = ln(sech.plus(1).div(number)) 'arc sech x = ln((a+1)/x)
        End Function
        Public Shared Function cosech(ByVal number As complex) As complex
            cosech = number.multi(number).plus(1).power(0.5) 'a=Å„(1+x^2)
            cosech = ln(cosech.plus(1).div(number)) 'arc cosech x = ln((a+1)/x)
        End Function
#End Region
    End Class
End Module

Module constant
#Region "ï®óùíËêî"

    Public Const k As Double = 0.0172029895 'Gauss Constant
    Public Const EGamma As Double = 0.577215664901533 'Euler Constant
    Public Const GGamma As Double = 0.000000066732 'ñúóLà¯óÕíËêîcm3/gs2
    Public Const astroA As Double = 149600000 'ìVï∂íPà km
    Public Const c As Double = 299792458 'åıë¨m/s
    Public Const ae As Double = 6378.16 'ínãÖê‘ìπîºåakm
    Public Const Jz As Double = 0.0033557046979865771 'ínãÖùGïΩó¶1/298
    Public Const GE As Double = 398603000000000.0 'ínêSèdóÕíËêîm3/s2
    Public Class moon 'åé
        Public Const mu As Double = 0.012300123001230012 'åéëŒínéøó î‰1/81.3
        Public Const n As Double = 0.000002661699489 'åéëŒçPïΩãœâ^ìÆ/s
        Public Const p As Double = 5025.64 'åéçŒç∑â^ìÆ sec.
    End Class
    Public Const eta1900 As Double = 23.452294444444444 'â©ìπåXéŒ deg
    Public Const N As Double = 9.21 'è’ìÆíËêî sec.
    Public Const sun As Double = 2.0 'ëæózíËêîcal/min.cm2
    Public Const Boltsmannk As Double = 1.380662E-23 'Boltsmann Constant J/K
    Public Const Stefan_Boltsmann_sigma As Double = 0.0000000567032 'Stefan=Boltsman Constant
    Public Const Plankh As Double = 6.626176E-34 'ÉvÉâÉìÉNíËêîJ/Hz
    Public Const Plankh_ As Double = 1.054589E-34 'ÉvÉâÉìÉNíËêîh_bar
    Public Const c1 As Double = 0.0000000000000003741832 'ëÊàÍï˙éÀíËêîWm2
    Public Const c2 As Double = 0.01438786 'ëÊìÒï˙éÀíËêîmK
    Public Const alpha As Double = 0.0072973506 'î˜ç◊ç\ë¢íËêî
    Public Const alpha_recip As Double = 137.03604 'î˜ç◊ç\ë¢íËêî
    Public Const R8 As Double = 1.097373177 'Rydberg Constant
    Public Const NA = 6.022045E+23 'Avogadro Number /mol
    Public Const F As Double = 96484.56 'Falad Constant C/mol
    Public Const R As Double = 8.31441 'ãCëÃíËêî J/mol K
    Public Const v1 As Double = 7.9 'ëÊàÍâFíàë¨ìxkm/s
    Public Const v2 As Double = 11.2 'ëÊìÒâFíàë¨ìxkm/s
    Public Const v3 As Double = 16.7 'ëÊéOâFíàë¨ìxkm/s
    Public Class m 'éøó kg
        Public Const mu As Double = 1.883566E-28
        Public Const p As Double = 1.6726485E-27
        Public Const n As Double = 1.6749543E-27
        Public Const e As Double = 9.109534E-31
    End Class
    Public Const u As Double = 1.6605655E-27 'å¥éqó kg
    Public Const e As Double = 1.6021892E-19 'ìdãCëfó C
    Public Const e2 As Double = 175880470000.0 'ìdéqî‰ìdâ◊ me C/kg
    Public Class mu 'é•ãC é•éq J/K
        Public Const p As Double = 1.4106171E-26
        Public Const mu As Double = 4.490474E-26
        Public Const N As Double = 5.050824E-27
        Public Const e As Double = 9.284832E-24
        Public Const B As Double = 9.274078E-24
    End Class
    Public Const a0 As Double = 0.000000000052917706 'É{Å[ÉAîºåa
    Public Class gamma 'é•éqäpâ^ìÆó î‰ /sT
        Public Const p As Double = 267519870.0
    End Class
    Public Class labmda 'ÉRÉìÉvÉgÉìîgí∑
        Public Const c As Double = 2.4263089
        Public Const cp As Double = 1.3214099
        Public Const cn As Double = 1.3195909
    End Class
    Public Const Vm As Double = 0.02241383 'óùëzãCëÃïWèÄëÃêœ m3/mol
    Public Const Phi0 As Double = 0.0000000000000020678506 'é•ë©ó éq Wb
    Public Const eta0 As Double = 0.00000000000885418782 'ê^ãÛóUìdó¶ F/m
#End Region
    Public Const CRTST2REpiEN = 0.79788456080286541
    Public Const CRTSTRE2piEN = 0.3989422804014327
    Public Const C2REpi = 0.63661977236758138
    Public Const C2MUpi = 6.2831853071795862
    Public Const CpiRE2 = 1.5707963267948966
    'RTST...EN ="Å„(...)"
    'RE="/"
    'PL="+"
    'MU="*"
    'MI="-"
    'ST...EN="(...)"
End Module
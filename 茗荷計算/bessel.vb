Module bessel
#Region "Private"
    Private Function u(ByVal n As Integer, ByVal t As Double) As Double
        Select Case n
            Case 0
                Return 1
            Case 1
                Return (-5 * t * t + 3) * t / 24
            Case 2
                t *= t
                Return ((385 * t - 462) * t + 81) * t / 1152
            Case 3
                Dim tt = t * t
                Return (((-425425 * tt + 765765) * tt - 369603) * tt + 30375) * t / 414720
            Case 4
                t *= t
                Return ((((185910720 * t - 446185740) * t + 34992243) * t - 94121676) * t + 4465125) * t / 38813120
            Case Else
                MsgBox("–¢’è‹`")
                Return 1
        End Select
    End Function
    Private Function y0a(ByVal x As Double) As Double
        y0a = (2 / pi) * log(x / 2)
        x /= 3
        x *= x
        y0a *= ((((((0.00021 * x + -0.0039444) * x + 0.0444479) * x + -3.163866) * x + 1.2656208) * x + -2.2499997) * x + 1) * x + 1
        y0a += ((((((-0.00024846 * x + 0.00427916) * x + -0.04261214) * x + 0.25300117) * x + -0.74350384) * x + 0.60559366) * x + 0.36746691) * x + 0.36746691
    End Function
    Private Function j0a(ByVal x As Double) As Double
        x /= 3
        x *= x
        Return ((((((0.00021 * x + -0.0039444) * x + 0.0444479) * x + -3.163866) * x + 1.2656208) * x + -2.2499997) * x + 1) * x + 1
    End Function
    Private Function y1a(ByVal x As Double) As Double
        y1a = (2 / pi) * log(x / 2)
        Dim xx = x
        x /= 3
        x *= x
        xx = (((((((0.0027873 * x + -0.0400976) * x + 0.3123951) * x + -1.3164827) * x + 2.1682709) * x + 0.2212091) * x + -0.6366198) * x + -0.6366198) / xx
        y1a = y1a * (((((((0.00001109 * x + -0.00031761) * x + 0.00443319) * x + -0.03954289) * x + 0.21093573) * x + -0.56249985) * x + 0.5) * x + 0.5) + xx
    End Function
    Private Function j1a(ByVal x As Double) As Double
        x /= 3
        x *= x
        Return ((((((0.00001109 * x + -0.00031761) * x + 0.00443319) * x + -0.03954289) * x + 0.21093573) * x + -0.56249985) * x + 0.5) * x + 0.5
    End Function
    Private Function y0b(ByVal x As Double) As Double
        Return f0a(x) * System.Math.Sin(t0a(x)) / System.Math.Sqrt(x)
    End Function
    Private Function f0a(ByVal x As Double) As Double
        x = 3 / x
        Return ((((((0.00014476 * x + -0.00072805) * x + 0.00137237) * x + -0.00009512) * x + -0.0055274) * x + -0.00000077) * x + 0.79788456) * x + 0.79788456
    End Function
    Private Function t0a(ByVal x As Double) As Double
        t0a = x
        x = 3 / x
        t0a += ((((((0.00013558 * x + -0.00029333) * x + -0.00054125) * x + 0.00262573) * x + -0.00003954) * x + -0.04166397) * x + -0.78539816) * x + -0.78539816
    End Function
    Private Function y1b(ByVal x As Double) As Double
        Return f1a(x) * System.Math.Sin(t1a(x)) / System.Math.Sqrt(x)
    End Function
    Private Function f1a(ByVal x As Double) As Double
        x = 3 / x
        Return ((((((-0.00020033 * x + 0.00113653) * x + -0.00249511) * x + 0.00017105) * x + 0.01659667) * x + 0.0000156) * x + 0.79788456) * x + 0.79788456
    End Function
    Private Function t1a(ByVal x As Double) As Double
        t1a = x
        x = 3 / x
        t1a += ((((((-0.00029166 * x + 0.00079824) * x + 0.00074348) * x + -0.00637879) * x + 0.0000565) * x + 0.12499612) * x + -2.35619449) * x + -2.35619449
    End Function
    Private Sub pq(ByRef p As Double, ByRef q As Double, ByVal n As Double, ByVal x As Double)
        n += 0.5
        Dim l = n * n / x
        If 14 < l Then
            l = Microsoft.VisualBasic.Int((0.55 + 0.0005 * n) * l + 7.5)
        ElseIf 3.5 < l Then
            l = Microsoft.VisualBasic.Int(0.77 * l + 4.6)
        ElseIf 0.08 < l Then
            l = Microsoft.VisualBasic.Int(1.4 * l + 2.6)
        Else
            l = 1
        End If
        x *= x
        Dim ƒ¢p As Double = 1
        Dim ƒ¢q As Double = 1 / (n * (n - 1))
        Dim c = 0
        p += ƒ¢p
        Do
            If c = l Then Exit Do
            c += 2
            ƒ¢p *= (n + c - 1) * (n + c - 2) * (n - c) * (n - c + 1) / (-4 * x * (c - 1) * c)
            ƒ¢q *= (n + c) * (n + c - 1) * (n - c - 1) * (n - c) / (-4 * x * c * (c + 1))
            p += ƒ¢p
            q += ƒ¢q
        Loop
        q *= 0.5 / x
    End Sub
#End Region
    Public Overloads Function Jn(ByVal n As Integer, ByVal x As Integer) As Double
        If n < 0 Then Return (-1) ^ (-n) * Jn(-n, x)
        Dim absx = System.Math.Abs(x)
        If absx <= 0.00002 Then
            Dim x2 = x / 2
            Dim nn = fctl(n)
            Jn = x2 ^ n * (1 / nn - 1 / (nn * (n + 1)) * x2 * x2)
        Else

            Dim am As Double = 0
            Dim aa As Double = 1.0E-75

            Dim m As Integer = System.Math.Max(n, x) + 14
            If 100 <= absx Then
                m += 33 + Microsoft.VisualBasic.Int(0.073 * absx)
            ElseIf 10 <= absx Then
                m += 13 + Microsoft.VisualBasic.Int(0.27 * absx)
            ElseIf 1 < absx Then
                m += Microsoft.VisualBasic.Int(1.4 * absx)
            End If

            If n = m Then Return am
            If n = m + 1 Then Return aa
            Dim ap As Double
            Dim jna

            If m Mod 2 = 1 Then
                ap = 2 * m / x * aa - am '‘Q‰»Ž®0
                m -= 1
                If m = n Then Jn = ap
                If m = 0 Then
                    jna = ap
                    GoTo exdo
                End If
                am = aa
                aa = ap
            End If
            Do
                ap = 2 * m / x * aa - am '‘Q‰»Ž®1
                m -= 1
                If m = n Then Jn = ap

                am = 2 * m / x * ap - aa '‘Q‰»Ž®2
                m -= 1
                If m = n Then Jn = am
                jna += am
                If m = 0 Then
                    jna *= 2
                    jna += am
                    Exit Do
                End If

                aa = 2 * m / x * am - ap '‘Q‰»Ž®3
                m -= 1
                If m = n Then Jn = aa

                ap = 2 * m / x * aa - am '‘Q‰»Ž®4
                m -= 1
                If m = n Then Jn = ap
                jna += ap
                If m = 0 Then
                    jna *= 2
                    jna += ap
                    Exit Do
                End If

                am = 2 * m / x * ap - aa '‘Q‰»Ž®5
                m -= 1
                If m = n Then Jn = am

                aa = 2 * n / x * am - ap '‘Q‰»Ž®6
                m -= 1
                If m = n Then Jn = aa
                jna += aa
                If m = 0 Then
                    jna *= 2
                    jna += aa
                    Exit Do
                End If
            Loop
exdo:
            Jn /= jna
        End If
    End Function
    Public Overloads Function Jn(ByVal n As Double, ByVal x As Double) As Double
        If n < 0 Then MsgBox("Jn;n > 0 È x > 0 >>> 'x –’‚Í n ‚É‚Â‚¢‚Ä–¢’è‹`'") : Return 1
        If x = 0 Then
            If n = 0 Then Return 1
            Return 0
        ElseIf (0 < x And x < 4) Or (n > 30 And n > 5 * (x - 10)) Then
            x /= 2
            Jn = x ^ n
            n += 1
            Dim y = x * x / n
            Dim l
            If 0.00035 <= y Then
                l = Microsoft.VisualBasic.Int(10 * System.Math.Sqrt(y) + 2.5)
            ElseIf 0.00000006 <= y Then
                l = 1
            Else
                l = 0
            End If
            x *= x
            Dim s = 0
            Dim k = 0
            Dim gamma = 1 / ƒ¡(n)
            Dim xx = 1
            Dim fck = 1
            Do
                s += xx * fck * gamma
                If k = l Then Exit Do
                k += 1
                fck /= k
                gamma /= n + k
                xx *= -x
            Loop
            Jn *= s
        ElseIf 100 <= x And n * n <= 30 * x Then
            Dim p, q : pq(p, q, n, x)
            n = x + (0.25 - 0.5 * n) * pi
            Jn = CRTST2REpiEN / System.Math.Sqrt(x) * (p * System.Math.Cos(n) + q * System.Math.Sin(n))
        ElseIf 100 <= x And 30 * x <= (x - n) ^ 3 Then
            Dim alpha = Tri.Arc.cosh(n / x)
            Jn = 1

            Dim y = (n - x) ^ 3 / x
            Dim l = 2
            If y <= 1000 Then l += 3
            If y <= 10000 Then l += 1

            Dim c = 1
            Dim tnh = 1 / Tri.tanh(alpha)
            Dim nn = 1 / n

            Do
                Jn += u(c, tnh) * nn
                If c = l Then Exit Do
                c += 1
                nn /= n
            Loop
            Jn *= System.Math.Exp(n * (tnh - alpha)) * CRTSTRE2piEN / System.Math.Sqrt(n * tnh)
        Else
            Dim alpha = n - Microsoft.VisualBasic.Int(n)
            Dim am As Double = 0
            Dim aa As Double = 1.0E-75

            Dim m As Integer = System.Math.Max(n, x) + 14
            If 100 <= x Then
                m = Microsoft.VisualBasic.Int(System.Math.Max(n, x) + x * 0.25) + 30
            ElseIf 1.2 * x + 5.5 < n Then
                If x <= 12 Then
                    m = Microsoft.VisualBasic.Int(n) + 9
                Else
                    m = Microsoft.VisualBasic.Int(n) + 15
                End If
            Else
                m = Microsoft.VisualBasic.Int(1.25 * x) + 16
            End If
            Dim ma = alpha + m
            m *= 0.5

            If n = alpha + m Then Return am
            If n = alpha + m + 1 Then Return aa
            Dim ap As Double
            Dim jna

            Do
                ap = 2 * ma / x * aa - am
                m -= 0.5
                ma -= 1
                If ma = n Then Jn = ap
                jna += ap * ma * ƒ¡(alpha + m) / ƒ¡(m)
                If m = 0 Then Exit Do

                am = 2 * ma / x * ap - aa
                m -= 1
                If ma = n Then Jn = am
                jna += am * ma * ƒ¡(alpha + m) / ƒ¡(m)
                If m = 0 Then Exit Do

                aa = 2 * ma / x * am - ap
                m -= 1
                If ma = n Then Jn = aa
                jna += aa * ma * ƒ¡(alpha + m) / ƒ¡(m)
                If m = 0 Then Exit Do
            Loop
            Jn *= (x / 2) ^ n / jna
        End If
    End Function
    Public Overloads Function Yn(ByVal n As Integer, ByVal x As Integer) As Double
        If n < 0 Then Return (-1) ^ (-n) * Yn(-n, x)
        x = System.Math.Abs(x)
        Dim y0, y1
        If x <= 0.00002 Then
            Dim x2 = x / 2
            y0 = C2REpi * ((math.ln(x2) + GGamma) * (1 - x2 * x2) + x2 * x2)
            y1 = ((1 - 0.5 * x2 * x2) * x2 * y0 - C2REpi / x) / (1 - x2 * x2)
        Else

        End If
        Dim m '######################m‚ÌÝ’è
        Dim am As Double = y0
        Dim aa As Double = y1
        If m = 0 Then Return am
        If m = 1 Then Return aa
        Dim ap As Double
        Dim l As Integer = 1
        Do
            ap = 2 * m / x * aa - am
            l += 1
            If n = m Then Return ap
            am = 2 * m / x * ap - aa
            l += 1
            If n = m Then Return am
            aa = 2 * m / x * am - ap
            l += 1
            If l = m Then Return aa
        Loop
    End Function
    Public Overloads Function Yn(ByVal n As Double, ByVal x As Double) As Double

    End Function
End Module

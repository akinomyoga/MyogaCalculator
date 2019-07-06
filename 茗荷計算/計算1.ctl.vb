Public Module �v�Z1
#Region "�萔"
    Public Const ��p = 1.61803398874989 '
    Public Const lg2p = 0.918938533204673 'log 2 pi
    Public Const gammah = 1.7724538509055161 '��(1/2)=��(��)
    Public Const rt2pm1 = 0.398942280401433 '1/��(2��)
    Public Const rt5m1 = 0.447213595499958 '1/��(5)
    Public Const pi2 = 6.28318530717959 '      2��
    Public Const gyaku_pi = 0.318309886183791 '1/��
    Public Const ��m1_4 = 0.636619772367581 '  2/��
    Public Const ��m1_2 = 1.27323954473516 '   4/��
    Public Const rt��m1 = 0.564189583547756 '1/��(��)
#End Region

#Region "���z"
    Public Class t 'Student
        Public Shared Function den(ByVal �� As Integer, ByVal x As Double) As Double
            den = ��((�� + 1) / 2) * (1 + x * x / ��) ^ ((�� + 1) / -2) * rt��m1 / ��(�� / 2) / System.Math.Sqrt(��)
        End Function
        Public Shared Function down(ByVal �� As Double, ByVal x As Double) As Double
            down = 1 - Ix(�� / (�� + x * x), �� / 2, 0.5) / 2
        End Function
        Public Shared Function up(ByVal �� As Double, ByVal x As Double) As Double
            up = Ix(�� / (�� + x * x), �� / 2, 0.5) / 2
        End Function
    End Class
    Public Class normal
        Public Shared Function down(ByVal x As Double) As Double '�W�����K���z�����m��
            If x >= 0 Then
                down = 0.5 + ��a(0.5, x * x) / gammah / 2
            Else
                down = up(-x)
            End If
        End Function
        Public Shared Function up(ByVal x As Double) As Double '�W�����K���z�㑤�m��
            If x >= 0 Then
                up = 0.5 - ��a(0.5, x * x) / gammah / 2
            Else
                up = down(-x)
            End If
        End Function
        Public Shared Function den(ByVal x As Double) As Double '�W�����K���z
            den = rt2pm1 / exp(-x * x / 2)
        End Function
        Public Shared Function den2(ByVal m As Double, ByVal s As Double, ByVal x As Double) As Double '���K���z
            Dim a
            a = m - x
            den2 = rt2pm1 / s / exp(-a * a / 2 / s / s)
        End Function
    End Class
    Public Class Chi2
        Public Shared Function den(ByVal �� As Integer, ByVal �� As Double) As Double
            �� = �� / 2
            If �� < 0 Then �� = -��
            den = �� ^ (�� - 1) * exp(-�� / 2) / 2 ^ �� / ��((��))
        End Function
        Public Shared Function down(ByVal �� As Double, ByVal ��2 As Double) As Double
            down = ��a(�� / 2, ��2 / 2) / ��(�� / 2)
        End Function
        Public Shared Function up(ByVal �� As Double, ByVal ��2 As Double) As Double
            up = ��a(�� / 2, ��2 / 2) / ��(�� / 2)
        End Function
    End Class
    Public Class bnml
        Public Shared Function down(ByVal p As Double, ByVal n As Integer, ByVal k As Integer) As Double
            If k < 0 Then
                down = 0
            ElseIf k >= n Then
                down = 1
            Else
                down = Ix(1 - p, k + 1, n - k)
            End If
        End Function
        Public Shared Function up(ByVal p As Double, ByVal n As Integer, ByVal k As Integer) As Double
            If k <= 0 Then
                up = 1
            ElseIf k > n Then
                up = 0
            Else
                up = Ix(p, (k), n - k + 1)
            End If
        End Function
        Public Shared Function den(ByVal p As Double, ByVal n As Integer, ByVal k As Integer) As Double
            den = mCn((n), (k)) * p ^ k * (1 - p) ^ (n - k)
        End Function
    End Class
    Public Class logistic
        Public Function den(ByVal x As Double) As Double
            Dim a As Double = System.Math.Exp(-x)
            Dim b As Double = a + 1
            den = a / b / b
        End Function
        Public Function l(ByVal x As Double) As Double
            l = 1 / (1 + System.Math.Exp(-x))
        End Function
    End Class
    Public Class F
        Public Function den(ByVal ��1 As Double, ByVal ��2 As Double, ByVal x As Double) As Double
            Dim a As Double, b As Double, C As Double
            a = ��1 / 2
            b = ��2 / 2
            C = x * ��1 / ��2
            den = C ^ a / (1 + C) ^ (a + b) / x / ��(a, b)
        End Function
        Public Function down(ByVal ��1 As Double, ByVal ��2 As Double, ByVal F As Double) As Double
            If F <= 0 Then
                down = 0
            Else
                down = Ix(��1 / (��1 + ��2 / F), ��1 / 2, ��2 / 2)
            End If
        End Function
        Public Function up(ByVal ��1 As Double, ByVal ��2 As Double, ByVal F As Double) As Double
            If F <= 0 Then
                up = 1
            Else
                up = Ix(��2 / (��2 + ��1 / F), ��2 / 2, ��1 / 2)
            End If
        End Function

    End Class
    Public Class erf
        Public Function l(ByVal s As Double, ByVal x As Double) As Double
            l = rt2pm1 / s / exp(-x * x / 2 / s / s)
        End Function
        Public Function den(ByVal x As Double) As Double
            If x >= 0 Then
                den = ��a(0.5, x * x) / gammah
            Else
                den = -��a(0.5, x * x) / gammah
            End If
        End Function
        Public Function c(ByVal x As Double) As Double
            If x >= 0 Then
                c = ��a(0.5, x * x) / gammah
            Else
                c = 1 + ��a(0.5, x * x) / gammah
            End If
        End Function
    End Class

    Public Function Wei_den(ByVal �� As Double, ByVal x As Double) As Double
        Wei_den = 1 - exp(-x ^ ��)
    End Function
    Public Function Ix_den(ByVal x As Double, ByVal a As Double, ByVal b As Double)
        If a < 0 Then
            a = -a
        ElseIf a = 0 Then
            a = 1
        End If
        If b < 0 Then
            b = -b
        ElseIf b = 0 Then
            b = 1
        End If
        If x < 0 Then
            x = 0
        ElseIf x > 1 Then
            x = 1
        End If
        Ix_den = x ^ (a - 1) * (1 - x) ^ (b - 1) / Ix(x, a, b)
    End Function
    Public Function Cach_den(ByVal x As Double) As Double
        Cach_den = gyaku_pi * (1 + x * x)
    End Function
    Public Function Geo_den(ByVal p As Double, ByVal n As Integer) As Double
        Geo_den = p * (1 - p) ^ (n - 1)
    End Function
    Public Function ��_den(ByVal a As Double, ByVal x As Double) As Double
        If a < 0 Then a = -a
        ��_den = x ^ (a - 1) / ��(x) / exp(x)
    End Function
    Public Function Tri_den(ByVal x As Double) As Double
        If x < -1 Then
            x = -1
        ElseIf x > 1 Then
            x = 1
        End If
        Tri_den = 1 - System.Math.Abs(x)
    End Function
    Public Function Xp_den(ByVal x As Double) As Double
        If x < 0 Then x = -x
        Xp_den = exp(-x)
    End Function

#End Region

#Region "�ϕ�"
    Public Function Ea(ByVal a As Double, ByVal x As Double) As Double
        Ea = ��a(1 - a, x) * x ^ (a - 1)
    End Function '�w���ϕ�
    Public Function Si(ByVal x As Double) As Double
        Dim a As Double, b1 As Double, b2 As Double, b3 As Double, b4 As Double, x2 As Double, x4 As Double
        Dim c
        x2 = x * x
        x4 = x2 * x2
        a = 1
        b1 = x
        b2 = 1
        b3 = 18
        b4 = x2
        Do
            c = b1 * (b3 - b4) / b2 / a / b3
            Si += c
            a += 4
            b1 *= x4
            b2 *= XnFall(a, 4)
            b3 = a + 2
            b3 *= b3 * (a + 1)
            b4 = x2 * a
        Loop While c <> 0 And a <= 30000
    End Function
    Public Function si2(ByVal x As Double) As Double
        si2 = Si(x) - hlfpi
    End Function
    Public Function Ci(ByVal x As Double) As Double
        Dim a As Double, b1 As Double, b2 As Double, b3 As Double, b4 As Double, x2 As Double, x4 As Double
        Dim c
        x2 = x * x
        x4 = x2 * x2
        a = 4
        b1 = x4
        b2 = 24
        b3 = 180
        b4 = 4 * x2
        Do
            c = b1 * (b3 - b4) / b2 / a / b3
            Ci += c
            a += 4
            b1 *= x4
            b2 *= XnFall(a, 4)
            b3 = a + 2
            b3 *= b3 * (a + 1)
            b4 = x2 * a
        Loop While c <> 0 And a <= 30000
        Ci = Ci + EGamma + log(x) - x ^ 4 / 4
    End Function
#End Region

    Public Function ��(ByVal x As Double) As Double
        Dim d, r
        r = x
        If x Mod 2 = 0 Then
            r /= 2
            Do
                x /= 2
            Loop While x Mod 2 = 0
        End If
        d = 3
        Do While x >= d * d
            If x Mod d = 0 Then
                r *= (d - 1) / d
                Do
                    x /= d
                Loop While x Mod d = 0
            End If
            d += 2
        Loop
        �� = r
    End Function

    Public Function integral_xdx(ByVal �� As Double, ByVal �� As Double, ByVal index As Integer)
        index = index + 1
        If index > 1 Then index = 1
        integral_xdx = 1 / index * �� ^ index - 1 / index * �� ^ index
    End Function


#Region "Gamma"
    Public Function Yn(ByVal n As Integer, ByVal x As Double) As Double
        Dim a As Double, a1 As Double, a2 As Double
        Dim s As Integer, b As Double
        Dim m
        s = 1
        Do
            b = Jn(2 * s, x) / s
            If s Mod 2 = 0 Then b = -b
            a1 = a1 + b
            s = s + 1
        Loop While b <> 0 And s <= 30000
        a1 = a1 * ��m1_4 + Jn(0, x) * (log(x) + EGamma - 0.693147180559945) * ��m1_2
        s = 1
        b = 0
        Do
            b = 2 * s + 1
            b = Jn((b), x) * b / s / (s + 1)
            If s Mod 2 = 0 Then b = -b
            a2 = a2 + b
            s = s + 1
        Loop While b <> 0 And s <= 30000
        a2 = (a2 + Jn(1, x) * (log(x) + EGamma - 1.69314718055995) - Jn(0, x) / x) * ��m1_2
        If n = 0 Then
            Yn = a1
        ElseIf n = 1 Then
            Yn = a2
        ElseIf n > 1 Then
            For m = 2 To n
                a = 2 * (m - 1) * a2 - a1
                a1 = a2
                a2 = a
            Next m
            Yn = a
        ElseIf n < 0 Then
            For m = -1 To n Step -1
                a = 2 * (m - 1) * a1 - a2
                a2 = a1
                a1 = a
            Next m
            Yn = a
        End If
    End Function
    Public Function Tn(ByVal n As Integer, ByVal x As Double) As Double 'Bernoulli#
        Dim b1, b2, pow, cnt
        Dim a1, a2, letter, num1, len
        b1 = "1,0,"
        For cnt = 1 To n
            pow = 1
            len = Microsoft.VisualBasic.Len(b1) - 1
            Do While Not Mid(b1, len, 1) = ","
                len -= 1
            Loop
            len -= 1
            Do While Not len = 0
                letter = Mid(b1, len, 1)
                Select Case letter
                    Case ","
                        b2 = num1 * pow & "," & b2
                        pow += 1
                        num1 = ""
                    Case Else : num1 = letter & num1
                End Select
                len -= 1
            Loop
            b2 = num1 * pow & "," & b2
            num1 = ""
            b1 = ""
            a1 = 0
            a2 = 0
            len = Microsoft.VisualBasic.Len(b2) - 1
            Do While Not len = 0
                letter = Mid(b2, len, 1)
                Select Case letter
                    Case ","
                        b1 = (CDbl(num1) + a1) & "," & b1
                        a1 = a2
                        a2 = num1
                        num1 = ""
                    Case Else : num1 = letter & num1
                End Select
                len -= 1
            Loop
            b1 = num1 & "," & a2 & "," & (CDbl(num1) + a1) & "," & b1
            num1 = ""
            b2 = ""
        Next cnt
        len = Microsoft.VisualBasic.Len(b1) - 1
        If x = 0 Then
            Do While Not len = 0
                letter = Mid(b1, len, 1)
                Select Case letter
                    Case ","
                        Tn += num1
                        Exit Function
                    Case Else : num1 = letter & num1
                End Select
                len -= 1
            Loop
        End If
        pow = 1
        Do While Not len = 0
            letter = Mid(b1, len, 1)
            Select Case letter
                Case ","
                    Tn += num1 * pow
                    pow *= 2
                    num1 = ""
                Case Else : num1 = letter & num1
            End Select
            len -= 1
        Loop
        Tn += num1 * pow
    End Function 'Bernoulli'####����####
    Public Function Bn(ByVal n As Integer) As Double
        Dim m
        If n <= 0 Then
            Bn = 1
        ElseIf n = 1 Then
            Bn = -0.5
        ElseIf n Mod 2 = 1 Then
            Bn = 0
        Else
            Bn = n * �v�Z1.Tn(n - 1, 0)
            m = 4 ^ (n / 2)
            Bn /= m * (m - 1)
            If n Mod 4 = 0 Then Bn = -Bn
        End If
    End Function 'Bernoulli��'####����####
    Public Function Fn(ByVal n As Integer) As Double
        Fn = int(rt5m1 * ��p ^ n + 0.5)
    End Function 'Fibonatti��'####����####

    Public Function Ix(ByVal x As Double, ByVal a As Double, ByVal b As Double) As Double
        Dim p1 As Double, p2 As Double, q1 As Double, q2 As Double, p As Double, q As Double, r As Integer
        Dim pp As Double
        Dim c, d, e
        p1 = 1
        p2 = 1
        q1 = 1
        q2 = 0
        r = 2
        Do
            c = a + 2 * e
            If r Mod 2 = 0 Then
                e = int(r / 2)
                d = e * (b - e) / c / (c - 1)
            Else
                e = int(r / 2) + a
                d = -e * (e + b) / c / (c + 1)
            End If
            q = x * q2 * d + q1
            p1 = p1 / q
            p2 = p2 / q
            q2 = q1 / q
            q1 = 1
            pp = p
            p = x * p2 * d + p1
            p2 = p1
            p1 = p
            r = r + 1
        Loop While pp - p <> 0 And r <= 30000
        Ix = x ^ a * (1 - x) ^ b / a / p / ��(a, b)
    End Function
    Public Function ��(ByVal a As Double, ByVal b As Double)
        Dim n As Integer, d As Double, p As Double
        Dim c
        n = 1
        c = a + b
        Do
            d = d + p
            p = (1 / a ^ n + 1 / b ^ n - 1 / c ^ n) * Bn(n + 1) / n / (n + 1)
            n = n + 2
        Loop While p <> 0 And n <= 30000
        d = d + lg2p + log(a) * (a - 1) + log(b) * (b - 1) - log(c) * (c - 1)
        �� = exp(d)
    End Function
    Public Function ��x(ByVal x As Double, ByVal a As Double, ByVal b As Double) As Double 'beta
        ��x = Ix(x, a, b) * ��(a, b)
    End Function

    Public Function ��(ByVal x As Double) As Double
        If x = 1 Then Return 1
        If x <= 0 Then
            x = -x
            If Microsoft.VisualBasic.Int(x) = x Then MsgBox("��(" & -x & ")�͖���`�ł��B") : Return 1
            Return pi / (System.Math.Sin(pi * x) * ��(1 + x))
        End If
        Dim b = 1
        Do While x < 10
            b *= x
            x += 1
        Loop
        Dim a As Double = 1 / x
        Dim da As Double = 1 / x / x
        Dim a0 As Double = 4
        Dim n As Integer = 2
        b = lg2p - x + (x - 0.5) * math.ln(x) - math.ln(b)
        Dim b0 As Double
        '@@@@Bernoulli���v�Z�p�ϐ�@@@@@@@@
        Dim b1, b2, pow, cnt
        Dim a1, a2, letter, num1, len
        b1 = "1,0,"
        Dim cnt0 = 1, tn
        '@@@@@@@@@@@@@@@@@@@@@@@@@@
        Do
            b0 = b
            '######Bernoulli���v�Z###################
            For cnt = 1 To cnt0
                pow = 1
                len = Microsoft.VisualBasic.Len(b1) - 1
                Do While Not Mid(b1, len, 1) = ","
                    len -= 1
                Loop
                len -= 1
                Do While Not len = 0
                    letter = Mid(b1, len, 1)
                    Select Case letter
                        Case ","
                            b2 = num1 * pow & "," & b2
                            pow += 1
                            num1 = ""
                        Case Else : num1 = letter & num1
                    End Select
                    len -= 1
                Loop
                b2 = num1 * pow & "," & b2
                num1 = ""
                b1 = ""
                a1 = 0
                a2 = 0
                len = Microsoft.VisualBasic.Len(b2) - 1
                Do While Not len = 0
                    letter = Mid(b2, len, 1)
                    Select Case letter
                        Case ","
                            b1 = (CDbl(num1) + a1) & "," & b1
                            a1 = a2
                            a2 = num1
                            num1 = ""
                        Case Else : num1 = letter & num1
                    End Select
                    len -= 1
                Loop
                b1 = num1 & "," & a2 & "," & (CDbl(num1) + a1) & "," & b1
                num1 = ""
                b2 = ""
            Next cnt
            cnt0 = 2
            len = Microsoft.VisualBasic.Len(b1) - 1
            Do While Not len = 0
                letter = Mid(b1, len, 1)
                Select Case letter
                    Case ","
                        tn = num1
                        Exit Do
                    Case Else : num1 = letter & num1
                End Select
                len -= 1
            Loop
            num1 = ""
            '###################
            b += tn / (a0 * (a0 - 1) * (n - 1)) * a
            n += 2
            a *= -da
            a0 *= 4
        Loop While n <= 3000 And Not b = b0
        �� = System.Math.Exp(b)
    End Function '####����####

    Public Function sml��a(ByVal a As Double, ByVal x As Double) As Double
        Dim n As Integer
        Dim b, b0
        If a >= x + 1 Then
            sml��a = ��(x) - ��a(a, x)
        ElseIf a = 0 Then
            sml��a = 0
        Else
            Do
                b0 = sml��a
                b = a ^ n / XnRise(x, n + 1)
                n += 1
                sml��a += b
                If sml��a = b0 Or n > 30000 Then Return sml��a * System.Math.Exp(-a) * a ^ x
            Loop
        End If
    End Function
    Public Function ��a(ByVal a As Double, ByVal x As Double) As Double
        Dim b
        Dim n As Integer
        If a < x + 1 Then
            ��a = ��(x) - sml��a(a, x)
        Else
            Do
                b = ��a
                ��a += XnRise(1 - x, n) / (fctl(n + 1) * Laguarre(n, -x, -a) * Laguarre(n - 1, -x, -a))
                n += 1
            Loop While (Not b = ��a) And n <= 30000
            ��a = ��a * exp(-a) * a ^ x
        End If
    End Function

    Public Function Pa(ByVal a As Double, ByVal x As Double) As Double
        Pa = ��a(a, x) / ��(x)
    End Function
    Public Function Qa(ByVal a As Double, ByVal x As Double) As Double
        Qa = ��a(a, x) / ��(x)
    End Function
    Public Function ��(ByVal x As Double) As Double
        Dim n As Integer = 2, a As Double, b As Double
        n = 2
        Do
            b = ��
            �� -= a
            a = Bn(n) / n / x ^ n
            n += 2
        Loop While Not �� = b And n <= 30000
        �� = �� + log(x) - 0.5 / x
    End Function
    Public Function ��n(ByVal n As Integer, ByVal x As Double) As Double
        Dim m As Integer = 2
        Dim a As Double, b As Double
        Dim xx = x * x
        Dim xn As Double = xx
        Do
            b = ��n
            ��n = ��n + a
            a = Bn(m) * XnRise(m + 1, n - 1) / xn
            xn *= xx
            m += 2
        Loop While Not b = ��n And m <= 30000
        ��n = (��n + fctl(n - 1) + fctl((n)) / 2) / x ^ n
        If n Mod 2 = 0 Then ��n = -��n
    End Function


    Public Function ��(ByVal x As Integer) As Double
        If x Mod 2 = 0 Then �� = pi2 ^ x * System.Math.Abs(Bn(x)) / 2 / fctl((x)) : Exit Function
        Dim n As Integer = 1, a As Double
        Do
            a = ��
            �� += 1 / n ^ x
            n += 1
        Loop While Not a = �� And n <= 30000

    End Function
#End Region
End Module
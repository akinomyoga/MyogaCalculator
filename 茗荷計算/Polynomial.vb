Module polynomial
    Public Function Tn(ByVal n As Double, ByVal x As Double) As Double '‘æˆêŽíTschebyscheff(Chebyshev)‘½€Ž®
        Tn = System.Math.Cos(n * System.Math.Acos(x))
    End Function '‘æˆêŽíTschebyscheff(Chebyshev)‘½€Ž® Tn
    Public Function Un(ByVal n As Double, ByVal x As Double) As Double '‘æ“ñŽíTschebyscheff(Chebyshev)‘½€Ž®
        Un = System.Math.Sinh(n * System.Math.Acos(x))
    End Function '‘æ“ñŽíTschebyscheff(Chebyshev)‘½€Ž® Un^*
    Public Function Laguarre(ByVal m As Integer, ByVal x As Double, Optional ByVal a As Double = 0)
        If m < 1 Then MsgBox("–¢’è‹`") : Return 1
        Dim am As Double = 1
        Dim aa As Double = a + 1 - x
        Dim ap As Double
        Dim n As Integer = 1
        Do
            ap = ((2 * n + a - x - 3) * aa - (n + a - 2) * am) / (n - 1)
            n += 1
            If n = m Then Return ap
            am = ((2 * n + a - x - 3) * ap - (n + a - 2) * aa) / (n - 1)
            n += 1
            If n = m Then Return am
            aa = ((2 * n + a - x - 3) * am - (n + a - 2) * ap) / (n - 1)
            n += 1
            If n = m Then Return aa
        Loop
    End Function 'Laguarre(”†)‘½€Ž® Ln^(a)

    Public Function Hermite1(ByVal m As Integer, ByVal x As Double)
        Dim am As Double = 1
        Dim aa As Double = x
        Dim ap As Double
        Dim n As Integer = 1
        Do
            ap = x * aa - (n + 1) * am
            n += 1
            If n = m Then Return ap
            am = x * ap - (n + 1) * aa
            n += 1
            If n = m Then Return am
            aa = x * am - (n + 1) * ap
            n += 1
            If n = m Then Return aa
        Loop
    End Function 'Hermite‘½€Ž®’è‹`1 Hn
    Public Function Hermite2(ByVal m As Integer, ByVal x As Double)
        Dim am As Double = 1
        Dim aa As Double = 2 * x
        Dim ap As Double
        Dim n As Integer = 1
        Do
            ap = 2 * (x * aa - (n + 1) * am)
            n += 1
            If n = m Then Return ap
            am = 2 * (x * ap - (n + 1) * aa)
            n += 1
            If n = m Then Return am
            aa = 2 * (x * am - (n + 1) * ap)
            n += 1
            If n = m Then Return aa
        Loop
    End Function 'Hermite‘½€Ž®’è‹`2 Hn
    Public Function LegendreP(ByVal m As Integer, ByVal x As Double, Optional ByVal a As Integer = 0)

        If m = a Then

            Return (-1) ^ (a Mod 2) * fcfc(2 * a - 1) * (1 - x * x) ^ (a / 2)

        ElseIf m > a Then

            Dim am As Double = (-1) ^ (a Mod 2) * fcfc(2 * a - 1) * (1 - x * x) ^ (a / 2)
            Dim aa As Double = x * (2 * m + 1) * am
            If m = a + 1 Then Return aa
            Dim ap As Double
            Dim n As Integer = a + 1
            Do
                ap = (x * (2 * n + 1) * aa - (n + a) * am) / (n - a + 1)
                n += 1
                If n = m Then Return ap
                am = (x * (2 * n + 1) * ap - (n + a) * aa) / (n - a + 1)
                n += 1
                If n = m Then Return am
                aa = (x * (2 * n + 1) * am - (n + a) * ap) / (n - a + 1)
                n += 1
                If n = m Then Return aa
            Loop

        ElseIf m < a Then

            Dim aa As Double = (-1) ^ (a Mod 2) * fcfc(2 * a - 1) * (1 - x * x) ^ (a / 2)
            Dim am As Double = x * (2 * m + 1) * aa
            Dim ap As Double
            Dim n As Integer = a
            Do
                ap = (x * (2 * n + 1) * aa - (n - a + 1) * am) / (n + a)
                n -= 1
                If n = m Then Return ap
                am = (x * (2 * n + 1) * ap - (n - a + 1) * aa) / (n + a)
                n -= 1
                If n = m Then Return am
                aa = (x * (2 * n + 1) * am - (n - a + 1) * ap) / (n + a)
                n -= 1
                If n = m Then Return aa
            Loop

        End If

    End Function 'Legendre(”†)‘½€Ž® Pn~(a)
    Public Function LegendreQ(ByVal m As Integer, ByVal x As Double)
        Dim am As Double = 0.5 * System.Math.Log(System.Math.Abs((x + 1) / (x - 1)))
        Dim aa As Double = x * am - 1
        If m = 0 Then Return am
        If m = 1 Then Return aa
        Dim ap As Double
        Dim n As Integer = 1
        Do
            ap = -((-x * (2 * n + 1)) * aa + (n) * am) / (n + 1)
            n += 1
            If n = m Then Return ap
            am = -((-x * (2 * n + 1)) * ap + (n) * aa) / (n + 1)
            n += 1
            If n = m Then Return am
            aa = -((-x * (2 * n + 1)) * am + (n) * ap) / (n + 1)
            n += 1
            If n = m Then Return aa
        Loop
    End Function '‘æ“ñŽí(Qn)Legendre‘½€Ž® Qn
    Public Function Tschebysheff1(ByVal m As Integer, ByVal x As Double)
        Dim am As Double = 1
        Dim aa As Double = x
        If m = 0 Then Return am
        If m = 1 Then Return aa
        Dim ap As Double
        Dim n As Integer = 1
        Do
            ap = -((-2 * x) * aa + am)
            n += 1
            If n = m Then Return ap
            am = -((-2 * x) * ap + aa)
            n += 1
            If n = m Then Return am
            aa = -((-2 * x) * am + ap)
            n += 1
            If n = m Then Return aa
        Loop
    End Function
    Public Function Tschebysheff2(ByVal m As Integer, ByVal x As Double)
        Dim am As Double = 0
        Dim aa As Double = 1
        If m = 0 Then Return am
        If m = 1 Then Return aa
        Dim ap As Double
        Dim n As Integer = 1
        Do
            ap = -((-2 * x) * aa + am)
            n += 1
            If n = m Then Return ap
            am = -((-2 * x) * ap + aa)
            n += 1
            If n = m Then Return am
            aa = -((-2 * x) * am + ap)
            n += 1
            If n = m Then Return aa
        Loop
    End Function
End Module


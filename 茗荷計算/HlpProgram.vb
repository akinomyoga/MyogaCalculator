Module HelpPrograming
    Public Function polynomial_a(ByVal name As String, ByVal s1 As String, ByVal s2 As String, ByVal s3 As String, ByVal a0 As String, ByVal a1 As String)
        '多項式を計算するプログラムを出力
        ' 漸化式 A_(n-1)*s1 + A_n*s2 + A_(n+1)*s3 = 0
        ' A_0 A_1 から求める
        If Not Len(s1) = 0 Then s1 = "(" & s1 & ") * "
        If Not Len(s2) = 0 Then s2 = "(" & s2 & ") * "
        If Not Len(s3) = 0 Then s3 = " / (" & s3 & ")"
        FileOpen(1, "c:\tempkoichi\output.txt", OpenMode.Append)
        PrintLine(1, "public function " & name & "(byval m as integer, x as double)")
        PrintLine(1, "dim am as double = " & a0)
        PrintLine(1, "dim aa as double =" & a1)
        PrintLine(1, "if m=0 then return am")
        PrintLine(1, "if m=1 then return aa")
        PrintLine(1, "dim ap as double")
        PrintLine(1, "dim n as integer = 1")
        PrintLine(1, "do")
        PrintLine(1, "ap = - (" & s2 & " aa +" & s1 & "am)" & s3)
        PrintLine(1, "n += 1")
        PrintLine(1, "if n = m then return ap")
        PrintLine(1, "am = - (" & s2 & " ap +" & s1 & "aa)" & s3)
        PrintLine(1, "n += 1")
        PrintLine(1, "if n = m then return am")
        PrintLine(1, "aa = - (" & s2 & " am +" & s1 & "ap)" & s3)
        PrintLine(1, "n += 1")
        PrintLine(1, "if n = m then return aa")
        PrintLine(1, "loop")
        PrintLine(1, "end function")
        FileClose(1)
        MsgBox("public function " & name & "をc:\tempkoichi\output.txtに追加しました。")
    End Function
    Public Function polynomial_b(ByVal name As String, ByVal s1 As String, ByVal s2 As String, ByVal s3 As String, ByVal ax As String, ByVal a2 As String, ByVal x As String)
        '多項式を計算するプログラムを出力
        ' 漸化式 A_(n-1)*s1 + A_n*s2 + A_(n+1)*s3 = 0
        ' A_X A_(X+1) から降りて求める
        s2 = LTrim(s2) 's2
        If s2 = "" Then
            s2 = "-"
        Else
            If s2 = "-" Then
                s2 = ""
            Else
                Dim c = 2
                Dim l = Len(s2)
                Do
                    If c >= l Then '単項式だった場合
                        If Left(s2, 1) = "-" Then
                            s2 = StrReIns(s2, 1, "")
                        ElseIf Left(s2, 1) = "+" Then
                            s2 = StrReIns(s2, 1, "-")
                        Else
                            s2 = "-" & s2
                        End If
                        Exit Do
                    End If
                    If Mid(s2, c, 1) = "+" Or Mid(s2, c, 1) = "-" Then '多項式だった場合
                        s2 = "- (" & s2 & ")"
                        Exit Do
                    End If
                    c += 1
                Loop
                s2 &= " * "
            End If
        End If

        s3 = LTrim(s3) 's3
        If s3 = "" Then
            s3 = "-"
        Else
            If s3 = "-" Then
                s3 = ""
            Else
                Dim c = 2
                Dim l = Len(s3)
                Do
                    If c >= l Then '単項式だった場合
                        If Left(s3, 1) = "-" Then
                            s3 = StrReIns(s3, 1, "+")
                        ElseIf Left(s2, 1) = "+" Then
                            s3 = StrReIns(s3, 1, "-")
                        Else
                            s3 = "-" & s3
                        End If
                        Exit Do
                    End If
                    If Mid(s3, c, 1) = "+" Or Mid(s3, c, 1) = "-" Then '多項式だった場合
                        s3 = "- (" & s3 & ")"
                        Exit Do
                    End If
                    c += 1
                Loop
                s3 &= " * "
            End If
        End If

        If Not s1 = "" Then 's1
            If s1 = "-" Then
                s1 = "-1"
            Else
                Dim c = 1
                Dim l = Len(s1)
                Do
                    If Mid(s1, c, 1) = "+" Or Mid(s3, c, 1) = "-" Then s1 = "(" & s1 & ")" : Exit Do
                    If c = l Then Exit Do
                Loop
                s1 = " / " & s1
            End If
        End If
        If Not s1 = "" Then s1 = " / (" & s1 & ")"
        FileOpen(1, "c:\tempkoichi\output.txt", OpenMode.Append)
        PrintLine(1, "public function " & name & "(byval m as integer, x as double)")
        PrintLine(1, "dim am as double = " & a2)
        PrintLine(1, "dim aa as double = " & ax)
        PrintLine(1, "if m = " & x & " then return am")
        PrintLine(1, "if m = " & x & " + 1 then return aa")
        PrintLine(1, "dim ap as double")
        PrintLine(1, "dim n as integer = " & x)
        PrintLine(1, "do")
        PrintLine(1, "ap =  (" & s2 & " aa " & s3 & "am)" & s1)
        PrintLine(1, "n -= 1")
        PrintLine(1, "if n = m then return ap")
        PrintLine(1, "am =  (" & s2 & " ap " & s3 & "aa)" & s1)
        PrintLine(1, "n -= 1")
        PrintLine(1, "if n = m then return am")
        PrintLine(1, "aa =  (" & s2 & " am " & s3 & "ap)" & s1)
        PrintLine(1, "n -= 1")
        PrintLine(1, "if n = m then return aa")
        PrintLine(1, "loop")
        PrintLine(1, "end function")
        FileClose(1)
        MsgBox("public function " & name & "をc:\tempkoichi\output.txtに追加しました。")
    End Function
    Public Function polynomial_c(ByVal name, ByVal s1)
        Dim s0 = Split(s1, ",")
        Dim u = UBound(s0)
        If u = 0 Then

        ElseIf u = 1 Then

        ElseIf u > 1 Then
            Dim s = s0(0)
            Dim n = 1
            Do While n <= u
                s = "(" & s & " * x + " & s0(n) & ")"
                n += 1
            Loop
            s = s & " * x + " & s0(u)
            FileOpen(1, "c:\tempkoichi\output.txt", OpenMode.Append)
            PrintLine(1, "public function " & name & "(byval x)as double")
            PrintLine(1, "return " & s)
            PrintLine(1, "end function")
            FileClose(1)
        End If
    End Function

End Module

Module file
    Public Sub 九九表()
        Dim a, b, c
        FileOpen(1, path1 & "\九九表.txt", OpenMode.Output)
        Print(1, "     *九九表*")
        Print(1, "*| 1| 2| 3| 4| 5| 6| 7| 8| 9")
        For a = 1 To 9
            c = a
            For b = 1 To 9
                c = c & "|" & Format(a * b, "00")
            Next b
            Print(1, c)
        Next a
        Print(1, "   **** Creater : 茗荷計算 ****")
        FileClose(1)
        MsgBox(path1 & "\九九表.txtに保存されました！")
    End Sub
    Public Sub 平方根表()
        Dim a1, a, b, c
        FileOpen(1, path1 & "\平方根表.txt", OpenMode.Output)
        Print(1, "     *平方根表* 小数点以下第三位まで")
        Print(1, "   |   0|   1|   2|   3|   4|   5|   6|   7|   8|   9")
        For a = 1 To 9.9 Step 0.1
            a1 = int(a * 10) / 10
            c = a1
            If Len(c) = 1 Then c &= ".0"
            For b = a1 To a1 + 0.09 Step 0.01
                c &= "|" & Microsoft.VisualBasic.Int(System.Math.Sqrt(b) * 1000)
            Next b
            Print(1, c)
        Next a
        For a = 100 To 990 Step 10
            c = " " & a / 10
            a1 = a
            For b = a1 To a1 + 9
                c &= "|" & Microsoft.VisualBasic.Int(System.Math.Sqrt(b / 10) * 1000)
            Next b
            Print(1, c)
        Next a
        Print(1, "   **** Creater : 茗荷計算 ****")
        FileClose(1)
        MsgBox(path1 & "\平方根表.txtに保存されました！")
    End Sub
    Public Sub 九九表html()
        FileOpen(1, path1 & "\九九表.txt", OpenMode.Output)
        PrintLine(1, "<table><caption>九九表</caption>")
        PrintLine(1, "<tr><th>　</th><th>1</th><th>2</th><th>3</th><th>4</th><th>5</th><th>6</th><th>7</th><th>8</th><th>9</th></tr>")
        Dim a As Integer, b As Integer, c As String
        For a = 1 To 9
            c = "<tr><th>" & a & "</th>"
            For b = 1 To 9
                c = c & "<td>" & a * b & "</td>"
            Next b
            PrintLine(1, c & "</tr>")
        Next a
        PrintLine(1, "</table>")
        PrintLine(1, "**** Creater : 茗荷計算 ****")
        FileClose(1)
        MsgBox(path1 & "\九九表.txtに保存されました！")
    End Sub
End Module

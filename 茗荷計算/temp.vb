
Public Module temp
    Dim x, togglebutton1
    Dim command1, command2, command3, command4, command5, command6, command7, command8
    Dim form2, form3, list1, list2, text1
    Dim aa, aa0, aa1, bb, bb0, bb1, bba, bbb, bbx, cc, cc0, cc1, cc2, cc3, cc4, ddd, ee0, ff0, ff1, ff2, nn
    Dim b, b0, b3, c, c3
    Dim adir, adir0, c2a, cda2, cnt1, flnm, len0, n1, n2, tr, tst, tst2
    Dim commondialog1, statusbar1

    Public Class label
        Public text As String
        Public ToolTipText
    End Class

    Dim label1 As temp.label, labela As temp.label, label2 As temp.label, label3 As temp.label
    Dim label4(3) As temp.label, label5 As temp.label, label6 As temp.label

    Dim a1 As Double, a2 As Double, b1 As Integer, b2 As Integer, c1 As Integer, c2 As Integer
    Dim d1(1) As Integer
    Public path1
    Private Sub Command1_Click(ByVal Index As Integer)
        If label1.text = "0" Then label1.text = ""
        label1.text = label1.text & Index
    End Sub
    Private Sub Command2_Click(ByVal Index As Integer)
        On Error GoTo err
        Select Case Index
            Case 0 : label1.text = Microsoft.VisualBasic.Int(CDbl(label1.text)) & "."
            Case 1 : If Not label1.text = 0 Then label1.text &= "00"
            Case 2 : label1.text = "3.14159265358979323846"
            Case 3 : label1.text = -label1.text
            Case 4 : label1.text = 1 / label1.text
            Case 5 : label1.text = System.Math.Abs(CDbl(label1.text))
            Case 6 To 11
                If togglebutton1.Value = True Then label1.text *= 0.017453292519943295
                Select Case Index
                    Case 6 : label1.text = System.Math.Sin(label1.text)
                    Case 7 : label1.text = System.Math.Cos(label1.text)
                    Case 8 : label1.text = System.Math.Tan(label1.text)
                    Case 9 : label1.text = 1 / System.Math.Sin(label1.text)
                    Case 10 : label1.text = 1 / System.Math.Cos(label1.text)
                    Case 11 : label1.text = 1 / System.Math.Tan(label1.text)
                End Select
            Case 12 To 17
                x = label1.text
                Select Case Index
                    Case 12 : label1.text = System.Math.Atan(x / System.Math.Sqrt(-x * x + 1))
                    Case 13 : label1.text = System.Math.Atan(-x / System.Math.Sqrt(-x * x + 1)) + 2 * System.Math.Atan(1)
                    Case 14 : label1.text = System.Math.Atan(x)
                    Case 16 : label1.text = System.Math.Atan(x / System.Math.Sqrt(x * x - 1)) + System.Math.Sign((x) - 1) * (2 * System.Math.Atan(1))
                    Case 15 : label1.text = System.Math.Atan(x / System.Math.Sqrt(x * x - 1)) + (System.Math.Sign(x) - 1) * (2 * System.Math.Atan(1))
                    Case 17 : label1.text = System.Math.Atan(x) + 2 * System.Math.Atan(1)
                End Select
                If togglebutton1.Value = True Then label1.text = label1.text * 180 / 3.14159265358979
            Case 18 To 23
                If togglebutton1.Value = True Then label1.text = label1.text / 180 * 3.14159265358979
                x = labela.text
                Select Case Index
                    Case 18 : label1.text = (System.Math.Exp(x) - System.Math.Exp(-x)) / 2
                    Case 19 : label1.text = (System.Math.Exp(x) + System.Math.Exp(-x)) / 2
                    Case 20 : label1.text = (System.Math.Exp(x) - System.Math.Exp(-x)) / (System.Math.Exp(x) + System.Math.Exp(-x))
                    Case 22 : label1.text = 2 / (System.Math.Exp(x) + System.Math.Exp(-x))
                    Case 21 : label1.text = 2 / (System.Math.Exp(x) - System.Math.Exp(-x))
                    Case 23 : label1.text = (System.Math.Exp(x) + System.Math.Exp(-x)) / (System.Math.Exp(x) - System.Math.Exp(-x))
                End Select
            Case 24 To 29
                x = labela.text
                Select Case Index
                    Case 24 : label1.text = System.Math.Log(x + System.Math.Sqrt(x * x + 1))
                    Case 25 : label1.text = System.Math.Log(x + System.Math.Sqrt(x * x - 1))
                    Case 26 : label1.text = System.Math.Log((1 + x) / (1 - x)) / 2
                    Case 28 : label1.text = System.Math.Log((System.Math.Sqrt(-x * x + 1) + 1) / x)
                    Case 27 : label1.text = System.Math.Log((System.Math.Sign(x) * System.Math.Sqrt(x * x + 1) + 1) / x)
                    Case 29 : label1.text = System.Math.Log((x + 1) / (x - 1)) / 2
                End Select
                If togglebutton1.Value = True Then label1.text *= 57.295779513082323
        End Select
        Exit Sub
err:
        If Err.Number = 11 Then
            MsgBox("0�Ōv�Z�ł��܂���I")
        ElseIf Err.Number = 5 Then
            MsgBox("�L�蓾�鐔�l����͂��ĉ������I" & Chr(13) & "arcsystem.math.sin[system.math.cos]:0-1" & Chr(13) & "arcsystem.math.cosec[sec]:1-")
        Else
            MsgBox("�����̃G���[���N���Čv�Z�ł��܂���B�G���[�i���o�[" & Err.Number)
            Call Command3_Click()
        End If
    End Sub
    Private Sub Command3_Click() '������
        Dim a
        label1.text = 0
        a1 = 0 : a2 = 0
        label2.text = ""
        b1 = 0 : b2 = 0
        label3.text = ""
        For a = 0 To 3
            label4(a).text = "0"
        Next a
        label5.text = ""
        label6.text = ""
    End Sub
    Private Sub Command4_Click(ByVal Index As Integer)
        On Error GoTo err
        b3 = b2
        If b2 = 1 Then a2 = label1.text
        If b2 = 0 And Index <> 6 Then GoTo skip1
        If Index = 5 Then GoTo skip1
        b2 = 0
        Select Case b1
            Case 1 : label1.text = a1 + a2
            Case 2 : label1.text = a1 - a2
            Case 3 : label1.text = a1 * a2
            Case 4 : label1.text = a1 / a2
            Case 5 : label1.text = a1 ^ a2
            Case 6 : label1.text = a1 ^ (1 / a2)
            Case 7 : label1.text = a1 Mod a2
        End Select
skip1:
        Dim letter
        Select Case Index
            Case 0 : letter = "�{"
            Case 1 : letter = "�|"
            Case 2 : letter = "�~"
            Case 3 : letter = "��"
            Case 4 : letter = "�O"
        End Select
        Select Case Index
            Case 0 To 3, 7
                label3.text = letter
                label2.text = label1.text & letter & "? =[ ]"
                a1 = label1.text
                label1.text = 0
                b1 = Index + 1 : If b1 = 8 Then b1 = 5
                b2 = 1
            Case 4
                label3.text = "��"
                label2.text = "? ��" & label1.text & "=[ ]"
                a1 = label1.text
                label1.text = 0
                b1 = 6
                b2 = 1
            Case 9
                label3.text = "�c"
                label2.text = label1.text & "�� ?=���c[ ]"
                a1 = label1.text
                label1.text = 0
                b1 = 7
                b2 = 1
            Case 6
                label2.text = a2
                a1 = label1.text
            Case 5 : label1.text = exclam(int(CDbl(label1.text)))
            Case 8 : If b3 = 1 Then label1.text *= 0.01
        End Select
        Exit Sub
err:
        If Err.Number = 6 Then
            MsgBox("�����傫�����ďo���܂���B")
        Else
            MsgBox("�s���ȃG���[�ŏo���܂���B")
        End If
    End Sub
    Private Sub Command5_Click(ByVal Index As Integer)
        Select Case Index
            Case 0
                label1.text = 0
            Case Is < 5
                label1.text = label4(Index - 1).text
            Case Is < 9
                label4(Index - 5).text = 0
            Case 9
                label1.text = Left(label1.text, Len(label1.text) - 1)
                If label1.text = "" Then label1.text = 0
        End Select
    End Sub
    Public Function exclam(ByVal a As Integer)
        c = 1
        If a > 0 Then
            For b = 1 To a
                c = c * b
            Next b
        ElseIf a < 0 Then
            For b = -1 To a Step -1
                c = c * b
            Next b
        End If
        exclam = c
    End Function
    Private Sub Command6_Click(ByVal Index As Integer)
        label4(Index).text = label1.text
    End Sub
    Private Sub Command7_Click(ByVal Index As Integer)
        If Index < 4 Then
            label4(Index).text = label4(Index).text - -label1.text
        Else
            label4(Index - 4).text = label4(Index - 4).text - label1.text
        End If
    End Sub
    Private Sub Command8_Click(ByVal Index As Integer)
        On Error GoTo err
        Select Case Index
            Case 0 To 5
                label6.text = command8(Index).text
                label6.ToolTipText = command8(Index).ToolTipText
                If Index = 2 Then
                    label6.text = ""
                    label6.ToolTipText = ""
                    label1.text = exclam(label1.text - 1)
                    d1(1) = 1
                Else
                    c1 = Index
                    d1(0) = label1.text
                    label5.text = d1(0)
                    c2 = 0
                    label1.text = 0
                End If
            Case 6
                c2 = 1
                d1(c2) = label1.text
                Select Case c1
                    Case 3
                        label5.text = label5.text & "," & int(label1.text)
                        d1(c2) = d1(c2) * exclam(label1.text)
                        d1(0) = d1(0) - -label1.text
                        label1.text = 0
                        Exit Sub
                    Case 0
                        label1.text = int(exclam(d1(0)) / exclam(d1(0) - label1.text))
                    Case 1
                        label1.text = d1(0) ^ int(label1.text)
                    Case 4
                        label1.text = int(exclam(d1(0)) / exclam(d1(0) - label1.text) / exclam(label1.text))
                    Case 5
                        label1.text = exclam(d1(0) + label1.text - 1) / exclam(d1(0) - 1) / exclam(label1.text)
                End Select
                If label1.text = 0 Then MsgBox("���l���m���߂Ă��������I")
                c2 = 0
                label6.text = ""
                label6.ToolTipText = ""
                label5.text = ""
                d1(0) = 0
                d1(1) = 0
            Case 7
                If c1 = 3 Then
                    c2 = 0
                    label6.text = ""
                    label6.ToolTipText = ""
                    label5.text = ""
                    label1.text = int(exclam(d1(0)) / d1(1))
                    If label1.text = 0 Then MsgBox("���l���m���߂Ă��������I")
                    c2 = 0
                    label6.text = ""
                    label6.ToolTipText = ""
                    label5.text = ""
                    d1(0) = 0
                    d1(1) = 0
                End If
        End Select
        Exit Sub
err:
        MsgBox(Err.Number)
    End Sub
    Private Sub Form_Load2()
        path1 = CurDir()
        commondialog1.InitDir = path1
    End Sub


    Public Const help0 = "[1-9][0][00][.]:��������͂���{�^��" & Chr(13) & "[��]:�~���������" _
        & Chr(13) & "[| |]:�\������Ă��鐔�l���Βl�ɂ���" & Chr(13) & "[1/]:�\������Ă��鐔�l���t���ɂ���" _
        & Chr(13) & "[+-]:�\������Ă��鐔�l�̕�����ς���" & Chr(13) & "[!]:�\������Ă��鐔�l�i�l�̌ܓ������l�j���K�悷��" _
        & Chr(13) & "[+][-][�~][��]:�����揜�̌v�Z������" & Chr(13) & "[��]:�ݏ捪����i���捪�����͂��ĉ������j" _
        & Chr(13) & "[^]:�ݏ�ɂ���" & Chr(13) & "[�c]:����Z�̗]������߂�" _
        & Chr(13) & "[��]:�S�����ŕ\��(1/100)" & Chr(13) & "[=]:�������o��" _
        & Chr(13) & "[��]:�����������" & Chr(13) & "[C]:�\������Ă��鐔���������A�O�ɂ���" _
        & Chr(13) & "[AC]:�S�Ă̒l���n�߂ɖ߂�"
    Private Sub mnopen_Click()
        Dim a, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t
        Dim b(4) As Double
        Dim c(3)
        On Error GoTo errhand
        commondialog1.Filter = "�v�Z1�t�@�C��(*.�v�Z�P)|*.�v�Z1"
        commondialog1.ShowOpen()
        a = commondialog1.FileName
        If Right(a, 4) <> ".�v�Z1" Then a = a & ".�v�Z1"
        FileOpen(1, a, OpenMode.Input)
        d = LineInput(1)
        e = LineInput(1)
        f = LineInput(1)
        g = LineInput(1)
        h = LineInput(1)
        i = LineInput(1)
        j = LineInput(1)
        k = LineInput(1)
        l = LineInput(1)
        m = LineInput(1)
        n = LineInput(1)
        o = LineInput(1)
        p = LineInput(1)
        q = LineInput(1)
        r = LineInput(1)
        s = LineInput(1)
        t = LineInput(1)
        FileClose(1)
        b(0) = d : label1.text = b(0)
        b(1) = g : label4(0).text = b(1)
        b(2) = h : label4(1).text = b(2)
        b(3) = i : label4(2).text = b(3)
        b(4) = j : label4(3).text = b(4)
        label2.text = e : label3.text = f
        label5.text = k : label6.text = l
        a1 = m : a2 = n : b1 = o : b2 = p
        c1 = q : c2 = r : d1(0) = s : d1(1) = t
        Exit Sub
errhand:
        MsgBox("�������t�@�C����I��ł��������I")
    End Sub
    Private Sub mnsankaku_Click()
        form3.Show()
    End Sub
    Private Sub mnsave_Click()
        Dim a
        On Error GoTo err
        commondialog1.CancelError = True
        commondialog1.Filter = "�v�Z1�t�@�C��(*.�v�Z�P)|*.�v�Z1"
        commondialog1.ShowSave()
        a = commondialog1.FileName
        If Right(a, 4) <> ".�v�Z1" Then a = a & ".�v�Z1"
        FileOpen(1, a, OpenMode.Output)
        Print(1, label1.text)
        Print(1, label2.text)
        Print(1, label3.text)
        Print(1, label4(0).text)
        Print(1, label4(1).text)
        Print(1, label4(2).text)
        Print(1, label4(3).text)
        Print(1, label5.text)
        Print(1, label6.text)
        Print(1, a1)
        Print(1, a2)
        Print(1, b1)
        Print(1, b2)
        Print(1, c1)
        Print(1, c2)
        Print(1, d1(0))
        Print(1, d1(1))
        FileClose(1)
err:
        If Err.Number = 72755 Then
            MsgBox("���[�U�[�ɂ��L�����Z������܂���!", 64, "Canceled")
        Else
            MsgBox("error", 16, "erred")
        End If
    End Sub
    Private Sub mnsystem2_math_tani_Click()
        form2.Show()
    End Sub

    Private Sub ToggleButton1_Click()
        If togglebutton1.Value = True Then
            togglebutton1.text = "��"
        Else
            togglebutton1.text = "���W�A��"
        End If
    End Sub
End Module
Public Module temp2
    Dim x, togglebutton1
    Dim label1, label2, label3, label4, label5, label6, label
    Dim command1, command2, command3, command4, command5, command6, command7, command8
    Dim form2, form3, list1, list2, text1, text2
    Dim aa, aa0, aa1, bb, bb0, bb1, bba, bbb, bbx, cc, cc0, cc1, cc2, cc3, cc4, ddd, ee0, ff0, ff1, ff2, nn
    Dim a, b, b1, b2, b0, b3, c, c3, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t
    Dim adir, adir0, c2a, cda2, cnt1, flnm, len0, n1, n2, tr, tst, tst2
    Dim commondialog1, statusbar1

    Dim itemcnt As Integer, bdat(999), cdat(2, 999) As Double
    Dim �ő�P�ʐ� As Integer

    Private Sub Command1_Click()
        On Error GoTo errh
        If list2.ListCount > 0 Then
            commondialog1.Filter = "*.txt"
            commondialog1.CancelError = True
            commondialog1.ShowSave()
            b = commondialog1.FileName
            If Right(b, 4) <> ".txt" Then b = b & ".txt"
            FileOpen(1, b, OpenMode.Output)
            Print(1, text1.Text & list1.List(list1.ListIndex))
            For a = 0 To list2.ListCount - 1
                Print(1, "=" & list2.List(a))
            Next a
            Print(1, "*******����*******")
            FileClose(1)
        End If
        Exit Sub
errh:
        If Err.Number = 32755 Then
            MsgBox("հ�ް�ɂ�跬ݾق���܂����B")
        Else
            MsgBox("�\�����ʴװ�F" & Err.Number)
            On Error Resume Next
            FileClose(1)
        End If
    End Sub

    Private Sub form_load3()
        �ő�P�ʐ� = 1000
        'path1�̍X�V
        commondialog1.InitDir = path1
    End Sub

    Private Sub List1_Click()
        Call Henkan()
    End Sub

    Private Sub mnn1_Click()
        FileOpen(1, path1 & "\�V.�P�ʂP", OpenMode.Output)
        Print(1, "�V�����P�ʂP�t�@�C���ł��B�P�ʂ�[�L��]�Ƃ��̒P�ʓ������[��")
        Print(1, "����]���Ō��݂ɉ��s���Ȃ��珑�����݂܂��B�����炠��P�ʂP")
        Print(1, "�t�@�C�����Q�l�ɂ��Ă��������B")
        FileClose(1)
    End Sub

    Private Sub mnn2_Click()
        FileOpen(1, path1 & "\�V.�P�ʂQ", OpenMode.Output)
        Print(1, "�V�����P�ʂQ�t�@�C���ł��B�P�ʂ�[�L��]�Ƃ��̒P�ʓ�����̑傫")
        Print(1, "���̔��[���q],[����],[���������_�̐����i������Ȃ��ꍇ��0")
        Print(1, "�ɂ��Ă����΂悢�ł��B�j]�ŕ\�������A�i���ɒP�ʂQa�t�@�C����")
        Print(1, "�Q�l�ɂ��镨������܂��B�����炠��P�ʂQ�t�@�C�����Q�l�ɂ���")
        Print(1, "���������B�j����s��[,]�ŋ�؂��ď������݂܂��B")
        FileClose(1)
    End Sub

    Private Sub mnn3_Click()
        FileOpen(1, path1 & "\�V.�P�ʂQa", OpenMode.Output)
        Print(1, "�V�����P�ʂQa�t�@�C���ł��B�P�ʂ̐ړ����[�L��]�Ƃ��̐ړ���")
        Print(1, "������̑傫���̔��[���q],[����],[���������_�̐����i������")
        Print(1, "�Ȃ��ꍇ��0�ɂ��Ă����΂悢�ł��B�j]�ŕ\����������s��[,]��")
        Print(1, "��؂��ď������݂܂��B�����炠��P�ʂQa�t�@�C�����Q�l�ɂ���")
        Print(1, "���������B")
        FileClose(1)
    End Sub

    Private Sub mnsystem3_math_tanifl_Click()
        On Error GoTo errhand
        commondialog1.Filter = "�P�ʂP�t�@�C��(*.�P�ʂP)|*.�P�ʂP"
        commondialog1.CancelError = True
        commondialog1.ShowOpen()
        a = commondialog1.FileName
        Dim d As Integer : d = 0
        list1.Clear()
        FileOpen(1, a, OpenMode.Input)
        Do While Not EOF(1) And d < �ő�P�ʐ�
            bb = LineInput(1)
            cc = LineInput(1)
            bdat(d) = bb : cdat(0, d) = cc : list1.AddItem(bb)
            d = d + 1
        Loop
        FileClose(1)
        itemcnt = d
        label3.text = a
        label4.text = itemcnt & "��"
        Exit Sub
errhand:
        If Err.Number = 32755 Then
            MsgBox("���[�U�[�ɂ��L�����Z������܂���!")
        Else
            MsgBox("�\�����ʴװ:" & Err.Number & "...�������t�@�C����I��ł��������I")
        End If
    End Sub

    Private Sub mnsystem4_math_tanifl2_Click()
        On Error GoTo errhand
        commondialog1.Filter = "�P�ʂQ�t�@�C��(*.�P�ʂQ)|*.�P�ʂQ"
        commondialog1.CancelError = True
        commondialog1.ShowOpen()
        a = commondialog1.FileName
        adir = Left(a, Len(a) - Len(commondialog1.FileTitle))
        Dim d As Integer
        d = 0
        list1.Clear()
        FileOpen(1, a, OpenMode.Input) '1f
        Do While Not EOF(1) And d < �ő�P�ʐ� '2f
            Input(1, bb0)
            Input(1, cc0)
            Input(1, cc1)
            Input(1, cc2)
            Input(1, cc3)
            Input(1, cc4)
            '�P�ʖ�,���q,����,�����_(e+/-?),�ړ���̧�ق̗L��(0/1),�����߽
            If cc3 = 1 Then '3f
                Dim edat(49), fdat(2, 49) As Double, d2 As Integer, cc2a As Integer
                d2 = 0
                FileOpen(2, adir & cc4, OpenMode.Input) '4f
                Do While Not EOF(2) And d2 < 50 '5f
                    Input(2, ee0)
                    Input(2, ff0)
                    Input(2, ff1)
                    Input(2, ff2)
                    '�ړ���,�ړ��̔{��(���q,����),���������_�̈ʒu
                    edat(d2) = ee0
                    fdat(0, d2) = ff0
                    fdat(1, d2) = ff1
                    fdat(2, d2) = ff2
                    d2 = d2 + 1
                Loop '5e
                FileClose(2) '4e
                For cnt1 = 0 To d2 - 1 '4f
                    bdat(d) = edat(cnt1) & bb0
                    list1.AddItem(bdat(d))
                    cdat(0, d) = fdat(0, cnt1) * cc0
                    cdat(1, d) = fdat(1, cnt1) * cc1
                    cc2a = cc2
                    cdat(2, d) = fdat(2, cnt1) + cc2
                    d = d + 1
                Next cnt1 '4e
            Else '3m
                bdat(d) = bb0
                list1.AddItem(bdat(d))
                cdat(0, d) = cc0
                cdat(1, d) = cc1
                cdat(2, d) = cc2
                d = d + 1
            End If '3e
        Loop '2e
        FileClose(1) '1e
        itemcnt = d
        label3.text = a
        label4.text = itemcnt & "��"
        Exit Sub
errhand:
        If Err.Number = 32755 Then
            MsgBox("���[�U�[�ɂ��L�����Z������܂���!")
        Else
            MsgBox("�g���q���u�P�ʂQ�v�̃t�@�C����I��ł�������!�g���q���u�P�ʂQ�v�̃t�@�C����I��ł����̒��ӂ��\�������ꍇ���̃t�@�C���ɖ�肪����\��������܂��B")
            MsgBox("�\�����ʴװ" & Err.Number)
        End If
    End Sub

    Private Sub mnsystem_math_tanifl3_Click()
        'On Error GoTo errhand
        commondialog1.Filter = "�P�ʃt�@�C��(*.units)|*.units"
        commondialog1.CancelError = True
        commondialog1.ShowOpen()
        a = commondialog1.FileName
        adir = Left(a, Len(a) - Len(commondialog1.FileTitle))
        Dim d As Integer
        d = 0
        list1.Clear()
        Call read_file("", "", 1, 1, 0, adir, "\" & commondialog1.FileTitle, d) '�ďo
        itemcnt = d
        label3.text = a
        label4.text = itemcnt & "��"
        Exit Sub
errhand:
        If Err.Number = 32755 Then
            MsgBox("���[�U�[�ɂ��L�����Z������܂���!")
        Else
            MsgBox("�������t�@�C���@���@���̊܂܂�Ă��Ȃ��t�@�C����I��ł�������!")
            MsgBox("�\�����ʴװ" & Err.Number)
        End If
    End Sub

    Public Sub read_file(ByVal b0, ByVal b1, ByVal a0, ByVal a1, ByVal a2, ByVal adir, ByVal adir2, ByRef d)
        '>>>�p�X����>>>----------------------------------------------------------------------------
        If Right(adir, 1) = "\" Then adir = Left(adir, Len(adir) - 1)
        If Left(adir2, 1) = "." Then
            adir2 = Mid(adir2, 2)
            Do While Left(adir2, 1) = "."
                adir2 = Mid(adir2, 2)
                len0 = Len(adir)
                For n = len0 To 1
                    If Mid(adir, n, 1) = "\" And Not tst = "tr" Then tst = "tr" & n1 = n - 1
                Next n
                If tst = "tr" Then adir = Left(adir, n1)
                tst = ""
            Loop
        End If
        If Not Left(adir2, 1) = "\" Then adir2 = "\" & adir2
        adir = adir & adir2
        len0 = Len(adir)
        For n = len0 To 1
            If Mid(adir, n, 1) = "\" And Not tst = "tr" Then tst = "tr" & n1 = n - 1
        Next n
        If tr = "tr" Then tr = "" : adir0 = Left(adir, n1)
        '>>>To Open The File>>>--------------------------------------------------------------------
        flnm = FreeFile()
        FileOpen(flnm, adir, OpenMode.Input)
        Do While Not EOF(flnm) And d < �ő�P�ʐ�
            Do
                Input(flnm, bb1)
            Loop While Mid(bb1, 3, 1) = "!"
            Input(flnm, cc0)
            Input(flnm, cc1)
            Input(flnm, cc2)
            '>>>����ؗ�����>>>------------------
            Do
                len0 = Len(bb1)
                For n = 1 To len0
                    If Mid(bb1, n, 1) = ";" And Not tst = "tr" Then tst = "tr" : n1 = n
                Next n
                If tst = "tr" Then
                    tst = ""
                    bb1 = Mid(bb0, n + 1)
                    bb0 = Left(bb0, n - 1)
                    tst2 = "come back"
                Else
                    bb0 = bb1
                    tst2 = ""
                End If
                '------------------<<<����ؗ�����<<<
                len0 = Len(bb0)
                For n = 1 To len0
                    If Mid(bb0, n, 1) = "�e" And Not tst = "tr" Then tst = "tr" : n1 = n + 1
                Next n
                If tst = "tr" Then
                    '>>>�ڎ�����>>>-----------
                    For n = len0 To n1
                        If Mid(bb0, n, 1) = "�f" And Not tst = "trtr" Then tst = "trtr" : n2 = n - n1
                    Next n
                    If tst = "trtr" Then
                        bbx = Mid(bb0, n1, n2)
                        bba = Left(bb0, n1 - 2)
                        bbb = Mid(bb0, n1 + n2 + 1)
                        Call read_file(b0 & bba, bbb & b1, a0 * cc0, a1 * cc1, a2 + cc2, adir0, bbx, d) '�ďo
                    End If
                    tst = ""
                    '----------<<<�ڎ�����<<<
                Else
                    '>>>�o�^>>>---------------
                    bdat(d) = b0 & bb0 & b1
                    list1.AddItem(bdat(d))
                    cdat(0, d) = a0 * cc0
                    cdat(1, d) = a1 * cc1
                    cdat(2, d) = a2 + cc2
                    MsgBox(bdat(d) & " ### " & cdat(0, d) & " ### " & cdat(1, d) & " ### " & cdat(2, d))
                    d = d + 1
                    '---------------<<<�o�^<<<
                End If
            Loop While tst2 = "come back"
        Loop
        FileClose(flnm)
    End Sub

    Private Sub Text1_Change()
        Call Henkan()
    End Sub

    Public Sub Henkan()
        On Error GoTo errh
        Dim c As Double, d As Double
        If list1.ListIndex >= 0 Then '1f
            b0 = cdat(0, list1.ListIndex) 'b0=�O��x/
            b1 = cdat(1, list1.ListIndex) : If b1 = 0 Then b1 = 1 'b1=�O��/x
            b2 = cdat(2, list1.ListIndex) 'b2=�O�̏����_
            On Error GoTo ERRH2 : c = text1.Text : On Error GoTo errh : text1.Text = c 'c=�ϊ��O�̐��l
            list2.Clear()
            For a = 0 To itemcnt - 1 '2f
                Dim cda0 As Double, cda1 As Double
                cda0 = cdat(0, a) : If cda0 = 0 Then cda0 = 1 'cda0=���x/
                cda1 = cdat(1, a) : If cda1 = 0 Then cda1 = 1 'cda1=���/x
                cda2 = cdat(2, a) 'cda2=��̏����_
                ddd = c * b0 * cda1 / b1 / cda0 * (10 ^ (b2 - cda2))
                list2.AddItem(ddd & " " & bdat(a))
                'List2.AddItem (d / cda0 * cda1 e-cda2 & " " & bdat(a))
            Next a '2e
        End If '1e
ERRH2:
        Exit Sub
errh:
        MsgBox("error")
    End Sub
End Module
Public Module temp3
    Dim x, togglebutton1
    Dim label1, label2, label3, label4, label5, label6, label
    Dim command1, command2, command3, command4, command5, command6, command7, command8
    Dim form2, form3, list1, list2, text1, text2
    Dim aa, aa0, aa1, bb, bb0, bb1, bba, bbb, bbx, cc, cc0, cc1, cc2, cc3, cc4, ddd, ee0, ff0, ff1, ff2, nn
    Dim a, b, b0, b3, c, c3, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t
    Dim adir, adir0, c2a, cda2, cnt1, flnm, len0, n1, n2, tr, tst, tst2
    Dim commondialog1, statusbar1

    Dim a1a(5) As Boolean  '�l�������Ă��邩�ǂ���
    Dim near0 As Double, near180 As Double, pi As Double

    Private Sub Form_Load()
        near0 = 0.000000000000001
        near180 = 179.999999999999
        pi = 3.14159265358979
    End Sub


    Private Sub StatusBar1_PanelClick(ByRef Panel As System.Windows.Forms.Panel)
        If Panel.Text = "������" Then
            For a = 0 To 2
                text2(a).text = ""
            Next a
            For a = 0 To 5
                text1(a).text = ""
                a1a(a) = False
            Next a
            For a = 1 To 11
                label5(a).text = ""
            Next a
        End If
    End Sub

    Private Sub Text1_lostfocus(ByVal Index As Integer)
        On Error GoTo ERHA
        Dim b As Integer
        If text1(Index).text = "" Then a1a(Index) = False : Exit Sub
        a1a(Index) = True
        Select Case Index
            Case 0 To 2
                If text1(Index).text <= 0 Then text1(Index).text = near0
                If text1(Index).text >= 180 Then text1(Index).text = near180
                For a = 0 To 2 '>>>���͂���Ă���p�x�̐�������邩
                    If a1a(a) = True Then b = b + 1
                Next a
                If b >= 2 Then  '>>>��p����c��̊p�����߂�
                    For a = 2 To 1 Step -1
                        aa = (Index + a) Mod 3
                        If a1a(aa) = True Then b = aa 'b ���m�̊p�ԍ�
                    Next a
                    c = 3 - Index - b             'c ���m�̊p�ԍ�
                    Dim d1a As Double, d2 As Double
                    d1a = text1(Index).text        'd1=���͂����p�x
                    d2 = text1(b).text            'd2=���̊��m�̊p�x
                    If d1a + d2 >= 180 Then                                              '�O�p�`�̓�p�̘a<180��
                        d2 = near180 - d1a                                                 '                     '
                        If d2 = 0 Then d2 = near0 : d1a = d1a - near0 : text1(Index).text = d1a '�ō݂邱�ƂɈ��钲�� '
                    End If                                                              '                     '
                    text1(b).text = d2                                                  '                     '
                    text1(c).text = 180 - d1a - d2 : a1a(c) = True
                    Call Text1_lostfocus(3)
                End If
                For a = 0 To 2
                    If Not text1(a).text = "" Then text2(a).text = text1(a).text / 180 * pi
                Next a
            Case 3 To 5
                Dim c1 As Integer, c2 As Integer 'c1=���͂���Ă���p�̐� c2=���͂���Ă���ӂ̐�(c2a=���̔ԍ�)
                For a = 0 To 2
                    If a1a(a) = True Then c1 = c1 + 1
                Next a
                For a = 3 To 5
                    If a1a(a) = True Then c2 = c2 + 1 : c2a = a
                Next a
                Select Case c2
                    Case 1 '��ӂƂ��̗��[�̊p>�c��̕�
                        If c1 = 3 Then
                            d = text1(c2a).text / System.Math.Sin(text1(c2a - 3).text / 180 * pi)
                            a = (c2a + 1) Mod 3
                            text1(a + 3).text = d * System.Math.Sin(text1(a).text / 180 * pi) : a1a(a + 3) = True
                            a = (c2a + 2) Mod 3
                            text1(a + 3).text = d * System.Math.Sin(text1(a).text / 180 * pi) : a1a(a + 3) = True
                            Call date_showing()
                        End If
                    Case 2 '��ӂƂ��̚�p>�c��̕�
                        Dim c4(1) As Double, c5 As Integer, d1a As Double
                        For a = 3 To 5
                            If a1a(a) = False And a1a(a - 3) = True Then
                                c3 = a : d1a = text1(c3 - 3).text 'd1=��p c3=���m�Ӕԍ�
                            ElseIf a1a(a) = True Then
                                c4(c5) = text1(a).text : c5 = c5 + 1
                            Else
                                GoTo skip1
                            End If
                        Next a
                        text1(c3).text = System.Math.Sqrt(c4(0) ^ 2 + c4(1) ^ 2 - 2 * c4(0) * c4(1) * System.Math.Cos(d1a / 180 * pi)) : a1a(c3) = True
                        GoTo �O��
skip1:
                    Case 3 '�O��>�p
                        If text1(3).text > text1(4).text + text1(5).text Then text1(3).text = "" : a1a(3) = False : Call status1("��")
                        If text1(4).text > text1(5).text + text1(3).text Then text1(4).text = "" : a1a(4) = False : Call status1("��")
                        If text1(5).text > text1(3).text + text1(4).text Then text1(5).text = "" : a1a(5) = False : Call status1("��")
                        GoTo �O��2
skip2:
                End Select 'if b
        End Select
        Exit Sub

ERHA:
        text1(Index).text = ""
        a1a(Index) = False
        Exit Sub

�O��:     '�O�ӂ���c��̊p�����߂�
        For a = 0 To 2
            If a1a(a) = False Then
                aa0 = (a + 1) Mod 3 : aa1 = (a + 2) Mod 3
                If a1a(aa0) = True And a1a(aa1) = True Then
                    text1(a).text = 180 - text1(aa0).text - text1(aa1).text : a1a(a) = True
                Else
                    Dim d22 As Double, d20 As Double, d21 As Double, d4 As Double 'd2=�O�ӂ̒��� d4=system.math.cos(�p),�p(radian)
                    d22 = text1(a + 3).text
                    d20 = text1((a + 1) Mod 3 + 3).text
                    d21 = text1((a + 2) Mod 3 + 3).text
                    d4 = (d20 * d20 + (d21 + d22) * (d21 - d22)) / (2 * d20 * d21)
                    d4 = System.Math.Atan(-d4 / System.Math.Sqrt(1 - d4 * d4)) + 1.5707963267949  'pi/2
                    text1(a).text = d4 / pi * 180 : a1a(a) = True
                End If
            End If
        Next a
        Call date_showing()
        GoTo skip1
�O��2:
        For a = 0 To 2
            If a1a(a) = False Then
                aa0 = (a + 1) Mod 3 : aa1 = (a + 2) Mod 3
                If a1a(aa0) = True And a1a(aa1) = True Then
                    text1(a).text = 180 - text1(aa0).text - text1(aa1).text : a1a(a) = True
                Else
                    Dim d22 As Double, d20 As Double, d21 As Double, d4 As Double 'd2=�O�ӂ̒��� d4=system.math.cos(�p),�p(radian)
                    d22 = text1(a + 3).text
                    d20 = text1((a + 1) Mod 3 + 3).text
                    d21 = text1((a + 2) Mod 3 + 3).text
                    d4 = (d20 * d20 + (d21 + d22) * (d21 - d22)) / (2 * d20 * d21)
                    d4 = System.Math.Atan(-d4 / System.Math.Sqrt(1 - d4 * d4)) + 1.5707963267949  'pi/2
                    text1(a).text = d4 / pi * 180 : a1a(a) = True
                End If
            End If
        Next a
        Call date_showing()
        GoTo skip2
    End Sub

    Private Sub Text2_LostFocus(ByVal Index As Integer)
        If text2(Index).text = "" Then Exit Sub
        text1(Index).text = text2(Index).text * 180 / pi
        Call Text1_lostfocus(Index)
    End Sub

    Public Sub date_showing()
        Dim a As Double, b As Double, c As Double, aa As Double, ab As Double, ac As Double
        a = text1(3).text : aa = text1(0).text / 180 * pi
        b = text1(4).text : ab = text1(1).text / 180 * pi
        c = text1(5).text : ac = text1(2).text / 180 * pi
        label5(2).text = a + b + c
        s = label5(2).text / 2
        label5(1).text = System.Math.Sqrt(s * (s - a) * (s - b) * (s - c))
        label5(3) = (a / System.Math.Sin(aa) + b / System.Math.Sin(ab) + c / System.Math.Sin(ac)) / 6
        label5(5) = label5(1).text / s
        For n = 2 To 3
            nn = n * 2
            label5(nn).text = pi * label5(nn - 1).text * label5(nn - 1).text
        Next n
    End Sub

    Public Sub status1(ByVal text)
        Select Case text
            Case "��"
                text = "��ӂ̒�������ӂ̒�����菬�����Ă͎O�p�`����邱�Ƃ��ł��܂���!"
        End Select
        statusbar1.Panels(1).text = text
    End Sub
End Module

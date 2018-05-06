Imports System.Text
Imports System.Threading

Public Class Form1
    '' Aplicação se divide em 3 blocos:

    '' Bloco de funções gerais:
#Region "Funções"

    Private thread1 As Thread
    Private thread2 As Thread

    '' Finaliza Threads ao fechar APP
    Private Sub Form1_FormClosing(sender As Object, e As FormClosingEventArgs) Handles MyBase.FormClosing
        Try
            thread1.Abort()
            thread1.Suspend()
        Catch ex As Exception : End Try

        Try
            thread2.Abort()
            thread2.Suspend()
        Catch ex As Exception : End Try
    End Sub

    '' Contar quantos caracteres iguais tem na String:
    Public Function ContarCaractere(Caractere As String, Cadeia As String) As Integer
        Dim tmpCont As Integer = 0
        For Each C As Char In Cadeia.ToCharArray()
            If Caractere = C.ToString() Then tmpCont += 1
        Next

        Return tmpCont
    End Function

#End Region

    '' Bloco de funções do IPv4
#Region "IPv4"
    '' BOTAO CALCULAR IPv4:
    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        If TestarIPv4(TextBox1.Text) Then
            'MsgBox("IP Valido")
            ToolStripStatusLabel1.Text = "Calculando IPv4..."
            CalcularIPv4(TextBox1.Text)

        Else
            ToolStripStatusLabel1.Text = "IP IPv4 Inválido."
        End If
    End Sub

    '' TESTAR IPv4:
    Private Function TestarIPv4(ByVal ipTextBox As String)
        Try
            Dim ips() As String = ipTextBox.Split("/")
            Dim ip As String = ips(0)

            Dim cidr As Integer = Integer.Parse(ips(1))

            ' Se CIDR estiver errado:
            If cidr < 0 Or cidr > 32 Then
                Return False
            End If

            ' Cria o padrão Regex
            Dim padraoRegex As String = "^([1-9]|[1-9][0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5])(\." &
            "([0-9]|[1-9][0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5])){3}$"

            ' Cria o objeto Regex
            Dim verificador As New RegularExpressions.Regex(padraoRegex)

            ' Verifica se o recurso foi fornecido
            If ip = "" Then 'ip invalido
                Return False
            Else 'usa o método IsMatch Method para validar o regex
                Return verificador.IsMatch(ip, 0)
            End If

        Catch ex As Exception
            Return False
        End Try

    End Function

    '' CALCULAR IPv4:
    Private Sub CalcularIPv4(ByVal ipCompleto As String)
        Try
            Dim ips() As String = ipCompleto.Split("/")
            Dim ip As String = ips(0)

            Dim cidr As Integer = Integer.Parse(ips(1))

            Dim classe As String = "C"
            Dim mascaraDeRede As String = "0.0.0.0"
            Dim ipRede As String
            Dim broadCast As String
            Dim primeiroHost As String
            Dim ultimoHost As String
            Dim range As String
            Dim totalDeIPs As String
            Dim totalDeIPsParaUso As String


            ' Mascara de rede:
            Dim hosts As Integer = Math.Pow(2, (32 - cidr))
            Dim xTemp As Integer = (32 - cidr)
            Dim xTemp2 As Integer

            If xTemp = 0 Then : mascaraDeRede = "255.255.255.255"
            ElseIf xTemp = 8 Then : mascaraDeRede = "255.255.255.0"
            ElseIf xTemp = 16 Then : mascaraDeRede = "255.255.0.0"
            ElseIf xTemp = 24 Then : mascaraDeRede = "255.0.0.0"
            ElseIf xTemp = 32 Then : mascaraDeRede = "0.0.0.0" : End If

            If xTemp > 0 And xTemp < 8 Then
                xTemp2 = 256 - hosts
                mascaraDeRede = "255.255.255." & xTemp2
            ElseIf xTemp > 8 And xTemp < 16 Then
                xTemp2 = 256 - (hosts / 256)
                mascaraDeRede = "255.255." & xTemp2 & ".0"
            ElseIf xTemp > 16 And xTemp < 24 Then
                xTemp2 = 256 - (hosts / (256 * 256))
                mascaraDeRede = "255." & xTemp2 & ".0.0"
            Else 'if xTemp > 24 And xTemp < 32 Then
                xTemp2 = 256 - (hosts / (256 * 256 * 256))
                mascaraDeRede = "" & xTemp2 & ".0.0.0"
            End If

            '' IP da Rede:
            Dim ipRedeTemp() As String = ip.Split(".")
            If hosts < Integer.Parse(ipRedeTemp(3)) Then '' TESTAR -> CORRIGIR, não esta 100%
                ipRede = ipRedeTemp(0) & "." & ipRedeTemp(1) & "." & ipRedeTemp(2) & "." & ipRedeTemp(3) & "/" & cidr
            Else
                ipRede = ipRedeTemp(0) & "." & ipRedeTemp(1) & "." & ipRedeTemp(2) & ".0/" & cidr
            End If

            ' CLASSE
            Dim classeNr = Integer.Parse(ipRedeTemp(0))
            If classeNr > 0 And classeNr < 128 Then : classe = "A" : End If
            If classeNr > 127 And classeNr < 192 Then : classe = "B" : End If
            If classeNr > 191 And classeNr < 224 Then : classe = "C" : End If
            If classeNr > 223 And classeNr < 240 Then : classe = "D" : End If
            If classeNr > 239 Then : classe = "E" : End If

            '' BroadCast:
            Dim netMaskTemp() As String = mascaraDeRede.Split(".")
            For i = 0 To 3
                If netMaskTemp(i) = 255 Then : broadCast &= ipRedeTemp(i) : End If
                If netMaskTemp(i) = 0 Then : broadCast &= "255" : End If
                If netMaskTemp(i) > 0 And netMaskTemp(i) < 255 Then : broadCast &= 256 - netMaskTemp(i) : End If
                If i < 3 Then : broadCast &= "." : End If
            Next

            '' Primeiro Host:
            Dim pHTemp() As String = ipRede.Split(".")
            primeiroHost = pHTemp(0) & "." & pHTemp(1) & "." & pHTemp(2) & ".1"

            '' Ultimo Host:
            Dim uHTemp() As String = broadCast.Split(".")
            If uHTemp(3) = 0 Then
                ultimoHost = uHTemp(0) & "." & uHTemp(1) & "." & uHTemp(2) & ".0"
            Else
                ultimoHost = uHTemp(0) & "." & uHTemp(1) & "." & uHTemp(2) & "." & uHTemp(3) - 1
            End If

            '' Range:
            range = primeiroHost & " - " & ultimoHost

            '' Total de IPs:
            totalDeIPs = hosts

            '' Total de IPs para uso:
            totalDeIPsParaUso = hosts - 2

            '' CALCULAR SUB-REDES:
            Dim nrSubRedes As Integer = ComboBox1.SelectedItem
            Dim nrAdrSubRedes As Integer = totalDeIPs / nrSubRedes

            '' Apresenta as informações:
            RichTextBox2.Text = "CLASSE: " & classe & vbNewLine
            RichTextBox2.Text &= "Rede: " & ipCompleto & vbNewLine
            RichTextBox2.Text &= "Endereço: " & ip & vbNewLine
            RichTextBox2.Text &= "Prefixo CIDR: " & cidr & vbNewLine
            RichTextBox2.Text &= "Mascara de rede: " & mascaraDeRede & vbNewLine
            RichTextBox2.Text &= "IP Rede: " & ipRede & vbNewLine
            RichTextBox2.Text &= "IP BroadCast: " & broadCast & vbNewLine
            RichTextBox2.Text &= "PrimeiroHost: " & primeiroHost & vbNewLine
            RichTextBox2.Text &= "Ultimo Host: " & ultimoHost & vbNewLine
            RichTextBox2.Text &= "Range: " & range & vbNewLine
            RichTextBox2.Text &= "Total de IPs: " & totalDeIPs & vbNewLine
            RichTextBox2.Text &= "Total de IPs para uso: " & totalDeIPsParaUso & vbNewLine
            RichTextBox2.Text &= "Sub-redes: " & nrSubRedes & vbNewLine
            RichTextBox2.Text &= "Endereços por Sub-rede: " & nrAdrSubRedes & vbNewLine

            '' Apresentar sub-redes:
            RichTextBox3.Text = ""

            '' Informar usuário:
            ToolStripStatusLabel1.Text = "Calculando Sub-Redes..."

            '' Calcular sub-redes:
            thread1 = New Thread(AddressOf calcularSubRedesIPv4)
            thread1.Name = "Thread Calculo de SubRedes"
            nrSubRedesI = nrSubRedes
            primeiroHostI = primeiroHost
            nrAdrSubRedesI = nrAdrSubRedes

            GroupBox1.Enabled = False
            GroupBox6.Enabled = False

            thread1.Start()

            'For index = 1 To nrSubRedes
            '    RichTextBox3.Text &= "SubRede " & index & " / " & nrSubRedes & ". Nrº Hosts: " & nrAdrSubRedes & vbNewLine

            '    RichTextBox3.Text &= "IP Inicial: " & primeiroHost & vbNewLine
            '    primeiroHost = addHostAoIp(primeiroHost, nrAdrSubRedes - 1)

            '    RichTextBox3.Text &= "IP Final: " & primeiroHost & vbNewLine
            '    primeiroHost = addHostAoIp(primeiroHost, 1)

            '    RichTextBox3.Text &= "========================================" & vbNewLine

            'Next

            '' Informar usuário:


        Catch ex As Exception
            ToolStripStatusLabel1.Text = "Ocorreu um erro ao calcular IPv4."
        End Try

    End Sub

    Dim nrSubRedesI As Integer
    Dim primeiroHostI As String
    Dim nrAdrSubRedesI As Integer

    '' Thread 1 - Calcula subRedes IPv4:
    Public Sub calcularSubRedesIPv4()

        '' CIDR da Subrede:
        Dim cidrI As Integer = 32 - Math.Ceiling(Math.Log(nrAdrSubRedesI, 2))

        For index = 1 To nrSubRedesI
            Me.Invoke(Sub() addSubRedeIPv4("SubRede " & index & " / " & nrSubRedesI & ". Nrº Hosts: " & nrAdrSubRedesI))
            'RichTextBox3.Text &= "SubRede " & index & " / " & nrSubRedesI & ". Nrº Hosts: " & nrAdrSubRedesI & vbNewLine

            Me.Invoke(Sub() addSubRedeIPv4("IP Inicial: " & primeiroHostI & "/" & cidrI))
            'RichTextBox3.Text &= "IP Inicial: " & primeiroHostI & vbNewLine
            primeiroHostI = addHostAoIPv4(primeiroHostI, nrAdrSubRedesI - 1)

            Me.Invoke(Sub() addSubRedeIPv4("IP Final: " & primeiroHostI & "/" & cidrI))
            'RichTextBox3.Text &= "IP Final: " & primeiroHostI & vbNewLine

            primeiroHostI = addHostAoIPv4(primeiroHostI, 1)

            Me.Invoke(Sub() addSubRedeIPv4("========================================"))
            'RichTextBox3.Text &= "========================================" & vbNewLine

        Next

        Me.Invoke(Sub() ToolStripStatusLabel1.Text = "Tudo calculado, veja as informações.")
        Me.Invoke(Sub() GroupBox1.Enabled = True)
        Me.Invoke(Sub() GroupBox6.Enabled = True)

    End Sub

    '' Função da Thread IPv4:
    Public Sub addSubRedeIPv4(ByVal linha As String)
        RichTextBox3.Text &= linha & vbNewLine
    End Sub

    '' ADICIONAR Hosts ao IP - IPv4:
    Private Function addHostAoIPv4(ByVal IP As String, ByVal hosts As Integer)
        Dim ipInicialTemp() As String = IP.Split(".")
        Dim oct4 As Integer = Integer.Parse(ipInicialTemp(3))
        Dim oct3 As Integer = Integer.Parse(ipInicialTemp(2))
        Dim oct2 As Integer = Integer.Parse(ipInicialTemp(1))
        Dim oct1 As Integer = Integer.Parse(ipInicialTemp(0))

        oct4 = oct4 + hosts
        If oct4 >= 255 Then
            oct3 = oct3 + Math.Floor(oct4 / 255)
            oct4 = oct4 - Math.Floor(oct4 / 255) * 255
        End If

        If oct3 >= 256 Then
            oct2 = oct2 + Math.Floor(oct3 / 255)
            oct3 = oct3 - Math.Floor(oct3 / 255) * 255
        End If

        If oct2 >= 255 Then
            oct1 = oct1 + Math.Floor(oct2 / 255)
            oct2 = oct2 - Math.Floor(oct2 / 255) * 255
        End If

        'MsgBox("IP: " & oct1 & "." & oct2 & "." & oct3 & "." & oct4)

        Dim ipRetorno As String = oct1 & "." & oct2 & "." & oct3 & "." & oct4

        Return ipRetorno
    End Function

    '' Ao digitar algo na barra do IPv4:
    Private Sub TextBox1_KeyPress(sender As Object, e As KeyPressEventArgs) Handles TextBox1.KeyPress

        '' Demais verificações:
        If Char.IsDigit(e.KeyChar) Or e.KeyChar = "." Or e.KeyChar = "/" Or Char.IsControl(e.KeyChar) Then
            e.Handled = False
        Else
            e.Handled = True
        End If

        '' Não deixa colocar 2 barras:
        If e.KeyChar = "/" And TextBox1.Text.Contains("/") Then
            e.Handled = True '' Cancela
        End If

        'Não deixa colcoar 2 pontos juntos:
        If TextBox1.Text.Length > 0 Then
            If e.KeyChar = "." And TextBox1.Text.Last = "." Then
                e.Handled = True '' Cancela
            End If
        End If

        '' Deixa iniciar só com numero:
        If (e.KeyChar = "." Or e.KeyChar = "/") And TextBox1.Text.Length = 0 Then
            e.Handled = True
        End If

        '' Só pode conter 3 pontos:
        If e.KeyChar = "." And ContarCaractere(".", TextBox1.Text) = 3 Then
            e.Handled = True
        End If

        '' Adicionar pontos sozinho:
        'If TextBox1.Text.Length Mod 3 = 0 And TextBox1.Text.Length > 0 Then
        '    e.Handled = False
        '    TextBox1.Text &= "."
        '    TextBox1.Select(TextBox1.Text.Length, 0)
        'End If

    End Sub

    '' Quando campo do IPv4 mudar:
    Private Sub TextBox1_TextChanged(sender As Object, e As EventArgs) Handles TextBox1.TextChanged
        If TestarIPv4(TextBox1.Text) Then
            TextBox1.ForeColor = Color.DarkGreen
            ToolStripStatusLabel1.Text = "Esperando..."
            Button1.Enabled = True
        Else
            TextBox1.ForeColor = Color.DarkRed
            ToolStripStatusLabel1.Text = "Formato de IPv4 inválido"
            Button1.Enabled = False
        End If
        Try
            Dim ips() As String = TextBox1.Text.Split("/")
            Dim ip As String = ips(0)

            Dim cidr As Integer = Integer.Parse(ips(1))
            Dim hosts As Integer = Math.Pow(2, (32 - cidr))
            ComboBox1.Items.Clear()

            While hosts > 0
                ComboBox1.Items.Add(hosts)
                hosts = hosts / 2
            End While

            ComboBox1.SelectedIndex = 0

            'Testar 'se pode ter sub-redes, caso se cidr for muito baixo ou muito alto

            'http://www.subnet-calculator.com/cidr.php
        Catch ex As Exception

        End Try

    End Sub

    '' Ao precionar ENTER no IPv4:
    Private Sub TextBox1_KeyDown(sender As Object, e As KeyEventArgs) Handles TextBox1.KeyDown
        '' Se precionar ENTER
        If e.KeyData = Keys.Enter Then
            Button1.PerformClick()
        End If

    End Sub

#End Region

    '' Bloco de funções do IPv6
#Region "IPv6"

    '' BOTAO CALCULAR IPv6: #TERMINAR
    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click

        MsgBox("Em desenvolvimento...")
        Return

        If TestarIPv6(TextBox2.Text) Then
            ToolStripStatusLabel1.Text = "Calculando IPv6..."
            CalcularIPv6(TextBox2.Text)
        Else
            ToolStripStatusLabel1.Text = "IP IPv6 Inválido."
        End If
    End Sub

    '' TESTAR IPv6: #TERMINAR
    Private Function TestarIPv6(ByVal ipTextBox As String)
        Try
            Dim ips() As String = ipTextBox.Split("/")
            Dim ip As String = ips(0)

            Dim cidr As Integer = Integer.Parse(ips(1))

            ' Se CIDR estiver errado:
            If cidr < 0 Or cidr > 32 Then
                Return False
            End If

            ' Cria o padrão Regex
            Dim padraoRegex As String = "^([1-9]|[1-9][0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5])(\." &
            "([0-9]|[1-9][0-9]|1[0-9][0-9]|2[0-4][0-9]|25[0-5])){3}$"

            ' Cria o objeto Regex
            Dim verificador As New RegularExpressions.Regex(padraoRegex)

            ' Verifica se o recurso foi fornecido
            If ip = "" Then 'ip invalido
                Return False
            Else 'usa o método IsMatch Method para validar o regex
                Return verificador.IsMatch(ip, 0)
            End If

        Catch ex As Exception
            Return False
        End Try

    End Function

    '' CALCULAR IPv6: #TERMINAR
    Private Sub CalcularIPv6(ByVal ipCompleto As String)
        Try
            Dim ips() As String = ipCompleto.Split("/")
            Dim ip As String = ips(0)

            Dim cidr As Integer = Integer.Parse(ips(1))

            Dim classe As String
            Dim mascaraDeRede As String
            Dim ipRede As String
            Dim broadCast As String
            Dim primeiroHost As String
            Dim ultimoHost As String
            Dim range As String
            Dim totalDeIPs As String
            Dim totalDeIPsParaUso As String


            ' Mascara de rede:
            Dim hosts As Integer = Math.Pow(2, (32 - cidr))
            Dim xTemp As Integer = (32 - cidr)
            Dim xTemp2 As Integer

            If xTemp = 0 Then : mascaraDeRede = "255.255.255.255"
            ElseIf xTemp = 8 Then : mascaraDeRede = "255.255.255.0"
            ElseIf xTemp = 16 Then : mascaraDeRede = "255.255.0.0"
            ElseIf xTemp = 24 Then : mascaraDeRede = "255.0.0.0"
            ElseIf xTemp = 32 Then : mascaraDeRede = "0.0.0.0" : End If

            If xTemp > 0 And xTemp < 8 Then
                xTemp2 = 256 - hosts
                mascaraDeRede = "255.255.255." & xTemp2
            ElseIf xTemp > 8 And xTemp < 16 Then
                xTemp2 = 256 - (hosts / 256)
                mascaraDeRede = "255.255." & xTemp2 & ".0"
            ElseIf xTemp > 16 And xTemp < 24 Then
                xTemp2 = 256 - (hosts / (256 * 256))
                mascaraDeRede = "255." & xTemp2 & ".0.0"
            ElseIf xTemp > 24 And xTemp < 32 Then
                xTemp2 = 256 - (hosts / (256 * 256 * 256))
                mascaraDeRede = "" & xTemp2 & ".0.0.0"
            End If

            '' IP da Rede:
            Dim ipRedeTemp() As String = ip.Split(".")
            If hosts < Integer.Parse(ipRedeTemp(3)) Then
                '' CORRIGIR, não esta 100%
                ipRede = ipRedeTemp(0) & "." & ipRedeTemp(1) & "." & ipRedeTemp(2) & "." & ipRedeTemp(3) & "/" & cidr
            Else
                ipRede = ipRedeTemp(0) & "." & ipRedeTemp(1) & "." & ipRedeTemp(2) & ".0/" & cidr
            End If

            ' CLASSE
            Dim classeNr = Integer.Parse(ipRedeTemp(0))
            If classeNr > 0 And classeNr < 128 Then : classe = "A" : End If
            If classeNr > 127 And classeNr < 192 Then : classe = "B" : End If
            If classeNr > 191 And classeNr < 224 Then : classe = "C" : End If
            If classeNr > 223 And classeNr < 240 Then : classe = "D" : End If
            If classeNr > 239 Then : classe = "E" : End If

            '' BroadCast:
            Dim netMaskTemp() As String = mascaraDeRede.Split(".")
            For i = 0 To 3
                If netMaskTemp(i) = 255 Then : broadCast &= ipRedeTemp(i) : End If
                If netMaskTemp(i) = 0 Then : broadCast &= "255" : End If
                If netMaskTemp(i) > 0 And netMaskTemp(i) < 255 Then : broadCast &= 256 - netMaskTemp(i) : End If
                If i < 3 Then : broadCast &= "." : End If
            Next

            '' Primeiro Host:
            Dim pHTemp() As String = ipRede.Split(".")
            primeiroHost = pHTemp(0) & "." & pHTemp(1) & "." & pHTemp(2) & ".1"

            '' Ultimo Host:
            Dim uHTemp() As String = broadCast.Split(".")
            If uHTemp(3) = 0 Then
                ultimoHost = uHTemp(0) & "." & uHTemp(1) & "." & uHTemp(2) & ".0"
            Else
                ultimoHost = uHTemp(0) & "." & uHTemp(1) & "." & uHTemp(2) & "." & uHTemp(3) - 1
            End If

            '' Range:
            range = primeiroHost & " - " & ultimoHost

            '' Total de IPs:
            totalDeIPs = hosts

            '' Total de IPs para uso:
            totalDeIPsParaUso = hosts - 2

            '' CALCULAR SUB-REDES:
            Dim nrSubRedes As Integer = ComboBox1.SelectedItem
            Dim nrAdrSubRedes As Integer = totalDeIPs / nrSubRedes

            '' Apresenta as informações:
            RichTextBox2.Text = "CLASSE: " & classe & vbNewLine
            RichTextBox2.Text &= "Rede: " & ipCompleto & vbNewLine
            RichTextBox2.Text &= "Endereço: " & ip & vbNewLine
            RichTextBox2.Text &= "Prefixo CIDR: " & cidr & vbNewLine
            RichTextBox2.Text &= "Mascara de rede: " & mascaraDeRede & vbNewLine
            RichTextBox2.Text &= "IP Rede: " & ipRede & vbNewLine
            RichTextBox2.Text &= "IP BroadCast: " & broadCast & vbNewLine
            RichTextBox2.Text &= "PrimeiroHost: " & primeiroHost & vbNewLine
            RichTextBox2.Text &= "Ultimo Host: " & ultimoHost & vbNewLine
            RichTextBox2.Text &= "Range: " & range & vbNewLine
            RichTextBox2.Text &= "Total de IPs: " & totalDeIPs & vbNewLine
            RichTextBox2.Text &= "Total de IPs para uso: " & totalDeIPsParaUso & vbNewLine
            RichTextBox2.Text &= "Sub-redes: " & nrSubRedes & vbNewLine
            RichTextBox2.Text &= "Endereços por Sub-rede: " & nrAdrSubRedes & vbNewLine

            '' Apresentar sub-redes:
            RichTextBox3.Text = ""

            '' Informar usuário:
            ToolStripStatusLabel1.Text = "Calculando Sub-Redes..."

            '' Calcular sub-redes:
            For index = 1 To nrSubRedes
                RichTextBox3.Text &= "SubRede " & index & " / " & nrSubRedes & ". Nrº Hosts: " & nrAdrSubRedes & vbNewLine

                RichTextBox3.Text &= "IP Inicial: " & primeiroHost & vbNewLine
                primeiroHost = addHostAoIPv6(primeiroHost, nrAdrSubRedes - 1)

                RichTextBox3.Text &= "IP Final: " & primeiroHost & vbNewLine
                primeiroHost = addHostAoIPv6(primeiroHost, 1)

                RichTextBox3.Text &= "========================================" & vbNewLine

            Next

            '' Informar usuário:
            ToolStripStatusLabel1.Text = "Tudo calculado, veja as informações."

        Catch ex As Exception
            ToolStripStatusLabel1.Text = "Ocorreu um erro ao calcular IPv4."
        End Try

    End Sub

    '' Thread 2 - Calcula subRedes IPv4: #TERMINAR
    Public Sub calcularSubRedesIPv6()

    End Sub

    '' Função da Thread IPv6:
    Public Sub addSubRedeIPv6(ByVal linha As String)
        RichTextBox5.Text &= linha & vbNewLine
    End Sub

    '' ADICIONAR Hosts ao IP - IPv6: #TERMINAR
    Private Function addHostAoIPv6(ByVal IP As String, ByVal hosts As Integer)
        Dim ipInicialTemp() As String = IP.Split(".")
        Dim oct4 As Integer = Integer.Parse(ipInicialTemp(3))
        Dim oct3 As Integer = Integer.Parse(ipInicialTemp(2))
        Dim oct2 As Integer = Integer.Parse(ipInicialTemp(1))
        Dim oct1 As Integer = Integer.Parse(ipInicialTemp(0))

        oct4 = oct4 + hosts
        If oct4 >= 255 Then
            oct3 = oct3 + Math.Floor(oct4 / 255)
            oct4 = oct4 - Math.Floor(oct4 / 255) * 255
        End If

        If oct3 >= 256 Then
            oct2 = oct2 + Math.Floor(oct3 / 255)
            oct3 = oct3 - Math.Floor(oct3 / 255) * 255
        End If

        If oct2 >= 255 Then
            oct1 = oct1 + Math.Floor(oct2 / 255)
            oct2 = oct2 - Math.Floor(oct2 / 255) * 255
        End If

        'MsgBox("IP: " & oct1 & "." & oct2 & "." & oct3 & "." & oct4)

        Dim ipRetorno As String = oct1 & "." & oct2 & "." & oct3 & "." & oct4

        Return ipRetorno
    End Function

    '' Ao digitar algo na barra do IPv6: #TERMINAR
    Private Sub TextBox2_KeyPress(sender As Object, e As KeyPressEventArgs) Handles TextBox2.KeyPress

        '' Demais verificações:
        If Char.IsDigit(e.KeyChar) Or e.KeyChar = ":" Or e.KeyChar = "/" Or Char.IsControl(e.KeyChar) Then
            e.Handled = False
        Else
            e.Handled = True
        End If

        '' Não deixa colocar 2 barras:
        If e.KeyChar = "/" And TextBox1.Text.Contains("/") Then
            e.Handled = True '' Cancela
        End If

        'Não deixa colcoar 2 pontos juntos:
        If TextBox1.Text.Length > 0 Then
            If e.KeyChar = "." And TextBox1.Text.Last = "." Then
                e.Handled = True '' Cancela
            End If
        End If

        '' Deixa iniciar só com numero:
        If (e.KeyChar = ":" Or e.KeyChar = "/") And TextBox1.Text.Length = 0 Then
            e.Handled = True
        End If

        '' Só pode conter 3 pontos:
        If e.KeyChar = ":" And ContarCaractere(":", TextBox1.Text) = 7 Then
            e.Handled = True
        End If

        '' Adicionar pontos sozinho:
        'If TextBox1.Text.Length Mod 3 = 0 And TextBox1.Text.Length > 0 Then
        '    e.Handled = False
        '    TextBox1.Text &= "."
        '    TextBox1.Select(TextBox1.Text.Length, 0)
        'End If

    End Sub

    '' Quando campo do IPv6 mudar: #TERMINAR
    Private Sub TextBox2_TextChanged(sender As Object, e As EventArgs) Handles TextBox2.TextChanged
        If TestarIPv4(TextBox1.Text) Then
            TextBox1.ForeColor = Color.DarkGreen
            ToolStripStatusLabel1.Text = "Esperando..."
            Button1.Enabled = True
        Else
            TextBox1.ForeColor = Color.DarkRed
            ToolStripStatusLabel1.Text = "Formato de IPv4 inválido"
            Button1.Enabled = False
        End If
        Try
            Dim ips() As String = TextBox1.Text.Split("/")
            Dim ip As String = ips(0)

            Dim cidr As Integer = Integer.Parse(ips(1))
            Dim hosts As Integer = Math.Pow(2, (32 - cidr))
            ComboBox1.Items.Clear()

            While hosts > 0
                ComboBox1.Items.Add(hosts)
                hosts = hosts / 2
            End While

            ComboBox1.SelectedIndex = 0

            'Testar 'se pode ter sub-redes, caso se cidr for muito baixo ou muito alto

            'http://www.subnet-calculator.com/cidr.php
        Catch ex As Exception

        End Try

    End Sub

    '' Ao precionar ENTER no IPv6:
    Private Sub TextBox2_KeyDown(sender As Object, e As KeyEventArgs) Handles TextBox2.KeyDown
        '' Se precionar ENTER
        If e.KeyData = Keys.Enter Then
            Button2.PerformClick()
        End If

    End Sub

#End Region

End Class

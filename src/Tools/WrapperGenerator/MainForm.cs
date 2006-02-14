﻿// <file>
//     <copyright see="prj:///doc/copyright.txt"/>
//     <license see="prj:///doc/license.txt"/>
//     <owner name="David Srbecký" email="dsrbecky@gmail.com"/>
//     <version>$Revision$</version>
// </file>

using System;
using System.Collections.Generic;
using System.CodeDom;
using System.CodeDom.Compiler;
using System.Drawing;
using System.IO;
using System.Reflection;
using System.Windows.Forms;

using Microsoft.CSharp;

namespace WrapperGenerator
{
	public class MainForm : System.Windows.Forms.Form
	{
		string header = 
				"// <file>" + "\r\n" + 
				"//     <copyright see=\"prj:///doc/copyright.txt\"/>" + "\r\n" + 
				"//     <license see=\"prj:///doc/license.txt\"/>" + "\r\n" + 
				"//     <owner name=\"David Srbecký\" email=\"dsrbecky@gmail.com\"/>" + "\r\n" + 
				"//     <version>$Revision$</version>" + "\r\n" + 
				"// </file>" + "\r\n" +
				"\r\n";
		//string dllFileName = Assembly.GetExecutingAssembly().Location;
		string dllFileName = Path.Combine(Assembly.GetExecutingAssembly().Location, @"..\..\..\..\..\..\AddIns\AddIns\Misc\Debugger\Debugger.Core.dll");
		string saveDirectory = Path.Combine(Assembly.GetExecutingAssembly().Location, @"..\..\..\..\..\..\src\AddIns\Misc\Debugger\Debugger.Core\Project\Src\Wrappers");
		
		public MainForm()
		{
			InitializeComponent();
			
			CodeGenerator codeGenerator1 = new CorDebugGenerator(Assembly.LoadFile(dllFileName));
			textBox1.Text = new FileGenerator(codeGenerator1, header).SaveFiles(saveDirectory + @"\CorDebug");
			
			CodeGenerator codeGenerator2 = new CorSymGenerator(Assembly.LoadFile(dllFileName));
			textBox1.Text += new FileGenerator(codeGenerator2, header).SaveFiles(saveDirectory + @"\CorSym");
		}
		
		[STAThread]
		public static void Main(string[] args)
		{
			Application.EnableVisualStyles();
			Application.Run(new MainForm());
		}
		
		#region Windows Forms Designer generated code
		/// <summary>
		/// This method is required for Windows Forms designer support.
		/// Do not change the method contents inside the source code editor. The Forms designer might
		/// not be able to load this method if it was changed manually.
		/// </summary>
		private void InitializeComponent()
		{
			textBox1 = new System.Windows.Forms.TextBox();
			this.SuspendLayout();
			// 
			// MainForm
			// 
			this.AutoScaleDimensions = new System.Drawing.SizeF(9F, 21F);
			this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
			this.ClientSize = new System.Drawing.Size(949, 717);
			// 
			// textBox1
			// 
			textBox1.Dock = System.Windows.Forms.DockStyle.Fill;
			textBox1.Location = new System.Drawing.Point(0, 0);
			textBox1.Multiline = true;
			textBox1.Name = "textBox1";
			textBox1.ScrollBars = System.Windows.Forms.ScrollBars.Both;
			textBox1.Size = new System.Drawing.Size(949, 717);
			textBox1.TabIndex = 0;
			this.Controls.Add(textBox1);
			this.Name = "MainForm";
			this.Text = "MainForm";
			this.PerformLayout();
			this.ResumeLayout(false);
		}
		private System.Windows.Forms.TextBox textBox1;
		#endregion
	}
}

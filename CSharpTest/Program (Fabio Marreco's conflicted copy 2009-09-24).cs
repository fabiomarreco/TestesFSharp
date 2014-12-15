using System;

using System.Data.SqlClient;
using System.Data;
using System.Data.Common;
using System.Data.Sql;
using System.IO;
using System.Security.Principal;
using System.Net;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using ICSharpCode.SharpZipLib;
using ICSharpCode;
using ICSharpCode.SharpZipLib.Core;
using ICSharpCode.SharpZipLib.Zip;

namespace CSharpTest
{
    class Program
    {
        static void Main(string[] args)
        {
            WebClient client = new WebClient();

            client.DownloadFileCompleted += new System.ComponentModel.AsyncCompletedEventHandler(client_DownloadFileCompleted);

            //client.DownloadFileCompleted += new System.ComponentModel.AsyncCompletedEventHandler(client_DownloadFileCompleted);


            //var a = new System.Text.RegularExpressions.Group();
            //a.v

            //SqlConnection conn = new SqlConnection @"Data Source=svrrisk01\sql2k501;Initial Catalog=rcPhoenix312;User ID=produtos;Password=rsksql125"
            //new SqlDataAdapter().Fill (


            //WebClient client = new WebClient();
            string uri = @"ftp://ftp.bmf.com.br/ContratosPregaoFinal/BF090909.ex_";
            client.Credentials = new NetworkCredential("anonymous", "joedoe@doe.com");
            byte[] data = client.DownloadData(uri);

            //var zip = new Chilkat.Zip();
            //zip.OpenFromByteData(data);
            var zip = new Chilkat.Zip();
            zip.OpenZip(@"C:\TEMP\Nova pasta\BF090911.exe");


                //"C:\TEMP\Nova pasta\BF090911.exe"

            var a = zip.OpenFromWeb(uri);


            data = UnzipBytes(data);
            Console.WriteLine (Encoding.ASCII.GetString(data));
        }

        static void client_DownloadFileCompleted(object sender, System.ComponentModel.AsyncCompletedEventArgs e)
        {
        }


        private static string Unzip(Stream stream)
        {
            int length = (int)stream.Length;
            byte[] bytInputData = new byte[length];
            stream.Read(bytInputData, 0, length);
            //bytInputData = DeCompress(bytInputData);
            bytInputData = UnzipBytes(bytInputData);
            return Encoding.ASCII.GetString(bytInputData);
        }

        private static byte[] UnzipBytes(byte[] bytesToDecompress)
        {
            ZipEntry objZipEntry;
            ZipInputStream objZipInputStream = new ZipInputStream(new MemoryStream(bytesToDecompress));
            //objZipInputStream.
            MemoryStream outStream = new MemoryStream();
            while ((objZipEntry = objZipInputStream.GetNextEntry()) != null)
            {
                if (Path.GetFileName(objZipEntry.Name) != "")
                {
                    int intSize = 2048;
                    byte[] arrData = new byte[2048];
                    intSize = objZipInputStream.Read(arrData, 0, arrData.Length);
                    while (intSize > 0)
                    {
                        outStream.Write(arrData, 0, intSize);
                        intSize = objZipInputStream.Read(arrData, 0, arrData.Length);
                    }
                }
            }
            objZipInputStream.Close();

            byte[] outArr = outStream.ToArray();
            outStream.Close();
            return outArr;
        }

    }
}

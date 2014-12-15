module Visualize
#light
open System.Text.RegularExpressions;
open System;
open System.Drawing;
open NPlot;
open System.Net;
open System.Windows.Forms;
open System.Drawing;
open System.IO;
open System.Data;
open System.Windows.Forms;



let gridview source =     
    let form = new Form(Visible = true, TopMost = true)
    let grid = new DataGridView(Dock = DockStyle.Fill)
    form.Controls.Add (grid)
    grid.DataSource <- source |> Seq.toArray
    
let visualize_unit vetor = 
    vetor |> Seq.mapi (fun i v -> (i, v)) |> gridview
    
let visualize_matriz (m:Matrix<float>) = 
    let tb = new DataTable ("teste")
    do [for i in 1..(m.NumCols) do tb.Columns.Add(i.ToString()) |> ignore] |> ignore
    do [for j in 0..(m.NumRows-1) do  m.Row(j) |> Seq.map (fun v -> v :> obj) |> Seq.toArray |> tb.Rows.Add |> ignore] |> ignore
    let form = new Form(Visible = true, TopMost = true)
    let grid = new DataGridView(Dock = DockStyle.Fill)
    form.Controls.Add (grid)
    grid.DataSource <- tb 
    grid.AutoResizeColumns()
    
    
(*
let rec concatena (separador, lista) = 
          match lista with
          | h::[] -> h.ToString()
          | h::t -> h.ToString() + separador + concatena (separador, t)
          | [] -> ""

let pick_count sequencia new_count = 
    let length = Seq.length (sequencia)
    [ for i in 1..new_count -> (length / new_count) * i ]  
    |> Seq.map (fun i -> sequencia |> Seq.nth (i-1))


let plot curva = 
    let url = 
        let curva_filtrada  = pick_count curva  9
        let valores = curva_filtrada |> Seq.map snd |> Seq.to_list;
        let datas = curva_filtrada |> Seq.map fst |> Seq.to_list;
        String.Format (@"http://chart.apis.google.com/chart?cht=lc&chd=t:{0}&chs=800x300&chl={1}", concatena (",", valores), concatena ("|", datas))
    let form = new Form (Visible = true, TopMost = true, Size = new Drawing.Size (808, 327))
    form.Controls.Add (new PictureBox(Dock = DockStyle.Fill, ImageLocation = url))

type Grafico<'a> () = 
    member self.Titulo = "Título"
    member self.Curva:seq<'a * float> = Seq.empty

*)


let plot (curva:seq<'a * float>) = 
    let absssica = curva |> Seq.map fst |> Seq.toArray
    let ordena = curva |> Seq.map snd |> Seq.toArray
    let form = new Form (Visible = true, TopMost = true, Size = new Drawing.Size (808, 327))
    let surface = new NPlot.Windows.PlotSurface2D(Dock = DockStyle.Fill, Title = "Titulo")
    form.Controls.Add (surface);
    let lp = new NPlot.LinePlot(ordena, absssica, Pen = new Pen( Color.Blue, 3.0f ), Label = "Funcao");
    surface.Add (lp);
    surface.Refresh();
    surface;;

    (*
    
    type LinePlot (label: string) = 
    member self.Titulo = "Título"
    member self.Curva:seq<'a * float> = Seq.empty
    

let plotLines (titulo:string) (lines:seq<( curva:seq<'a * float>) = 
    let absssica = curva |> Seq.map fst |> Seq.toArray
    let ordena = curva |> Seq.map snd |> Seq.toArray
    let form = new Form (Visible = true, TopMost = true, Size = new Drawing.Size (808, 327))
    let surface = new NPlot.Windows.PlotSurface2D(Dock = DockStyle.Fill, Title = "Titulo")
    form.Controls.Add (surface);
    let lp = new NPlot.LinePlot(ordena, absssica, Pen = new Pen( Color.Blue, 3.0f ), Label = "Funcao");
    surface.Add (lp);
    surface.Refresh();;
    //surface;;
*)

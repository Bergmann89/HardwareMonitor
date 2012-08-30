unit uColorForm; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, StdCtrls;

type
  TbgraColor = packed record
    case Integer of
      0: (c: Cardinal);
      1: (b, g, r, a: Byte);
  end;

  TrgbaColor = packed record
    case Integer of
      0: (c: Cardinal);
      1: (r, g, b, a: Byte);
  end;

  { TColorForm }

  TColorForm = class(TForm)
    RedEd: TEdit;
    GreenEd: TEdit;
    BlueEd: TEdit;
    AlphaEd: TEdit;
    ColorEd: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    OkBt: TButton;
    AbortBt: TButton;
    RedPB: TPaintBox;
    GreenPB: TPaintBox;
    BluePB: TPaintBox;
    PreviewPB: TPaintBox;
    RedTB: TTrackBar;
    GreenTB: TTrackBar;
    BlueTB: TTrackBar;
    AlphaTB: TTrackBar;
    procedure ColorEdChange(Sender: TObject);
    procedure PreviewPBPaint(Sender: TObject);
    procedure RedEdChange(Sender: TObject);
    procedure RedTBChange(Sender: TObject);
  private
    fColor: TbgraColor;
  public
    property Color: TbgraColor read fColor;

    function ShowModalColor(const aColor: Cardinal): Integer;
  end; 

var
  ColorForm: TColorForm;

implementation

uses
  uUtils;

{$R *.lfm}

{ TColorForm }

procedure TColorForm.PreviewPBPaint(Sender: TObject);
var
  pb: TPaintBox;
  i: Integer;
  c: TrgbaColor;
begin
  pb := (Sender as TPaintBox);
  if Assigned(pb) then begin
    c.c := RGBToColor(fColor.r, fColor.g, fColor.b);
    c.a := 0;
    pb.Canvas.Pen.Width := 1;
    case pb.Tag of
      0: begin
        for i := 0 to pb.Width do begin
          c.r := Round(i/pb.Width*255);
          pb.Canvas.Pen.Color := c.c;
          pb.Canvas.MoveTo(i, 0);
          pb.Canvas.LineTo(i, pb.Height);
        end;
      end;
      1: begin
        for i := 0 to pb.Width do begin
          c.g := Round(i/pb.Width*255);
          pb.Canvas.Pen.Color := c.c;
          pb.Canvas.MoveTo(i, 0);
          pb.Canvas.LineTo(i, pb.Height);
        end;
      end;
      2: begin
        for i := 0 to pb.Width do begin
          c.b := Round(i/pb.Width*255);
          pb.Canvas.Pen.Color := c.c;
          pb.Canvas.MoveTo(i, 0);
          pb.Canvas.LineTo(i, pb.Height);
        end;
      end;
      3: begin
        pb.Canvas.Brush.Color := c.c;
        pb.Canvas.Brush.Style := bsSolid;
        pb.Canvas.Pen.Color   := clBlack;
        pb.Canvas.Pen.Width   := 1;
        pb.Canvas.Rectangle(0, 0, pb.Width, pb.Height);
      end;
    end;
  end;
end;

procedure TColorForm.ColorEdChange(Sender: TObject);

  procedure UpdateTB(const tb: TTrackBar; const aPos: Integer);
  var
    Event: TNotifyEvent;
  begin
    Event := tb.OnChange;
    tb.OnChange := nil;
    tb.Position := aPos;
    tb.OnChange := event;
  end;

  procedure UpdateEd(const ed: TEdit; const aText: String);
  var
    Event: TNotifyEvent;
  begin
    Event := ed.OnChange;
    ed.OnChange := nil;
    ed.Text := aText;
    ed.OnChange := event;
  end;

var
  i: Cardinal;
begin
  if not TryHexToInt(ColorEd.Text, i) then
    ColorEd.Font.Color := clRed
  else begin
    ColorEd.Font.Color := clBlack;
    fColor.c := i;
    UpdateTB(  RedTB, fColor.r);
    UpdateTB(GreenTB, fColor.g);
    UpdateTB( BlueTB, fColor.b);
    UpdateTB(AlphaTB, fColor.a);
    UpdateEd(  RedEd, IntToStr(fColor.r));
    UpdateEd(GreenEd, IntToStr(fColor.g));
    UpdateEd( BlueEd, IntToStr(fColor.b));
    UpdateEd(AlphaEd, IntToStr(fColor.a));
    RedPB.Repaint;
    GreenPB.Repaint;
    BluePB.Repaint;
    PreviewPB.Repaint;
  end;
end;

procedure TColorForm.RedEdChange(Sender: TObject);
var
  ed: TEdit;
  i: Integer;
begin
  ed := (Sender as TEdit);
  if Assigned(ed) then begin
    if not TryStrToInt(ed.Text, i) then
      ed.Font.Color := clRed
    else begin
      ed.Font.Color := clBlack;
      if i > 255 then
        i := 255;
      if i < 0 then
        i := 0;
      case ed.Tag of
        0: begin
          fColor.r := i;
          RedTB.Position := fColor.r;
        end;
        1: begin
          fColor.g := i;
          GreenTB.Position := fColor.g;
        end;
        2: begin
          fColor.b := i;
          BlueTB.Position := fColor.b;
        end;
        3: begin
          fColor.a := i;
          AlphaTB.Position := fColor.a;
        end;
      end;
      ColorEd.Text := IntToHex(fColor.c, 8);
    end;
  end;
end;

procedure TColorForm.RedTBChange(Sender: TObject);
var
  tb: TTrackBar;
begin
  tb := (Sender as TTrackBar);
  if Assigned(tb) then begin
    ColorEd.Text := IntToHex(fColor.c, 8);
    case tb.Tag of
      0: begin
        fColor.r := tb.Position;
        RedEd.Text := IntToStr(fColor.r);
      end;
      1: begin
        fColor.g := tb.Position;
        GreenEd.Text := IntToStr(fColor.g);
      end;
      2: begin
        fColor.b := tb.Position;
        BlueEd.Text := IntToStr(fColor.b);
      end;
      3: begin
        fColor.a := tb.Position;
        AlphaEd.Text := IntToStr(fColor.a);
      end;
    end;
  end;
  RedPB.Repaint;
  GreenPB.Repaint;
  BluePB.Repaint;
  PreviewPB.Repaint;
end;

function TColorForm.ShowModalColor(const aColor: Cardinal): Integer;
begin
  fColor.c := aColor;
  RedTB.Position   := fColor.r;
  GreenTB.Position := fColor.g;
  BlueTB.Position  := fColor.b;
  AlphaTB.Position := fColor.a;

  RedEd.Text   := IntToStr(fColor.r);
  GreenEd.Text := IntToStr(fColor.g);
  BlueEd.Text  := IntToStr(fColor.b);
  AlphaEd.Text := IntToStr(fColor.a);
  ColorEd.Text := IntToHex(fColor.c, 8);

  result := inherited ShowModal;
end;

end.


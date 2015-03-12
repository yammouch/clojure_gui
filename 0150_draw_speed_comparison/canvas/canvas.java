import javafx.application.Application;
import javafx.event.EventHandler;
import javafx.scene.Group;
import javafx.scene.Node;
import javafx.scene.Scene;
import javafx.scene.canvas.Canvas;
import javafx.scene.canvas.GraphicsContext;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyEvent;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.Pane;
import javafx.scene.paint.Color;
import javafx.stage.Stage;

public class canvas extends Application {
  double origin_x, origin_y, grid;
  Canvas canvas;
  GraphicsContext gc;

  void draw_square(GraphicsContext gc, double x, double y) {
    gc.strokeRect(x, y, grid, grid);
  }  

  void draw_circle(GraphicsContext gc, double x, double y) {
    gc.strokeOval(x, y, grid, grid);
  }

  void draw_triangle(GraphicsContext gc, double x, double y) {
    double[] xs = new double[3];
    double[] ys = new double[3];
    xs[0] = x; xs[1] = x + grid;     xs[2] = x;
    ys[0] = y; ys[1] = y + 0.5*grid; ys[2] = y + grid;
    gc.strokePolygon(xs, ys, 3);
  }

  void draw_and(GraphicsContext gc, double x, double y) {
    gc.beginPath();
    gc.moveTo(x, y);
    gc.lineTo(x + 0.5*grid, y);
    gc.arcTo( x + grid
            , y + 0.5*grid
            , x + 0.5*grid
            , y + grid
            , 0.5*grid );
    gc.lineTo(x, y + grid);
    gc.closePath();
    gc.stroke();
  }

  void draw_objects(GraphicsContext gc, double xorg, double yorg) {
    double x, y;
    y = yorg;
    for (int i = 0; i < 100; i++, y += grid) {
      x = xorg;
      for (int j = 0; j < 100; j++, x += grid) {
        int modulo = (j + i) % 4;
        switch (modulo) {
          case 0: draw_square(gc, x, y); break;
          case 1: draw_circle(gc, x, y); break;
          case 2: draw_triangle(gc, x, y); break;
          case 3: draw_and(gc, x, y); break;
        }
      }
    }
  }

  void installEventHandler(Canvas canvas) {
    EventHandler<KeyEvent> keyEventHandler =
    new EventHandler<KeyEvent>() {
      public void handle(KeyEvent keyEvent) {
        if      (keyEvent.getCode() == KeyCode.RIGHT) { origin_x += grid; }
        else if (keyEvent.getCode() == KeyCode.LEFT ) { origin_x -= grid; }
        else if (keyEvent.getCode() == KeyCode.UP   ) { origin_y -= grid; }
        else if (keyEvent.getCode() == KeyCode.DOWN ) { origin_y += grid; }
        //switch (keyEvent.getCode()) {
        //  case KeyCode.RIGHT: origin_x += grid; break;
        //  case KeyCode.LEFT : origin_x -= grid; break;
        //  case KeyCode.UP   : origin_y -= grid; break;
        //  case KeyCode.DOWN : origin_y += grid; break;
        //}
        gc.clearRect(0.0, 0.0, canvas.getWidth(), canvas.getHeight());
        draw_objects(gc, origin_x, origin_y);
        keyEvent.consume();
      }
    };

    canvas.setOnKeyPressed(keyEventHandler);
  }

  public void start(Stage stage) {
    origin_x = origin_y = 0.0; grid = 8.0;
    canvas = new Canvas(1024.0, 768.0);
    gc = canvas.getGraphicsContext2D();
    BorderPane topgroup = new BorderPane();
    gc.setStroke(Color.BLACK);
    gc.setFill(Color.TRANSPARENT);
    installEventHandler(canvas);
    canvas.setFocusTraversable(true);
    draw_objects(gc, origin_x, origin_y);

    topgroup.setCenter(canvas);
    stage.setWidth(1024);
    stage.setHeight(768);
    stage.setScene(new Scene(topgroup));
    stage.setTitle("Draws a lot of objects");
    stage.show();

    canvas.requestFocus();
  }

  public static void main(String[] args) {
    launch(args);
  }
}

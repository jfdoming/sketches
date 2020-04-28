float exp_re(float x, float y) {
  return exp(x) * cos(y);
}

float exp_im(float x, float y) {
  return exp(x) * sin(y);
}

float f_re(float x, float y) {
  //if (abs(y - x * x) < 0.1) return 1;
  //if (x < 0) return 0.5;
  //return y < 0 ? 1 : 0;//*/return sin(tan(x)+1.0/tan(y));
  //return exp(-abs(y * sin(x)));
  float re = exp_re(x, y) + 1;
  float im = exp_im(x, y);
  return re / (re * re + im * im);
}

float f_im(float x, float y) {
  //if (abs(y) < 2 / PI) return 0;
  //return exp(-abs(sin(x) / tan(1 / y)));
  float re = exp_re(x, y) + 1;
  float im = exp_im(x, y);
  return -im / (re * re + im * im);
}

void setup() {
  size(500, 500);
  colorMode(HSB, 255);
}

float shift_x = 0;
float shift_y = 0;
float shift = 0.1;
float zoom = 16;
boolean dragging = false;

void draw() {
  loadPixels();
  float w = zoom;
  float h = zoom;
  float dx = w / width;
  float dy = h / height;
  
  float min_re = 0;
  float max_re = 0;
  float min_im = 0;
  float max_im = 0;
  
  float x = -w / 2 + shift_x;
  for (int i = 0; i < width; i++) {
    float y = h / 2 + shift_y;
    for (int j = 0; j < height; j++) {
      float val_re = f_re(x, y);
      if (i == 0 && j == 0) {
        min_re = max_re = val_re;
      }
      if (val_re < min_re) {
        min_re = val_re;
      }
      if (val_re > max_re) {
        max_re = val_re;
      }
      
      float val_im = f_im(x, y);
      if (i == 0 && j == 0) {
        min_im = max_im = val_im;
      }
      if (val_im < min_im) {
        min_im = val_im;
      }
      if (val_im > max_im) {
        max_im = val_im;
      }
      
      y -= dy;
    }
    x += dx;
  }
  
  float dist_re = max_re - min_re;
  if (dist_re == 0) {
    dist_re = 1;
  }
  
  float dist_im = max_im - min_im;
  if (dist_im == 0) {
    dist_im = 1;
  }
  
  x = -w/2 + shift_x;
  for (int i = 0; i < width; i++) {
    float y = h / 2 + shift_y;
    for (int j = 0; j < height; j++) {
      float val_re = f_re(x, y);
      float col_re = (1 - ((max_re - val_re) / dist_re)) * 255.0;
      float val_im = f_im(x, y);
      float col_im = (1 - ((max_im - val_im) / dist_im)) * 255.0;
      pixels[i + j * width] = color(col_im, 255, col_re);
      y -= dy;
    }
    x += dx;
  }
  
  updatePixels();
  
  stroke(255);
  float render_x = -shift_x * width / zoom + width / 2.0;
  float render_y = shift_y * height / zoom + height / 2.0;
  line(render_x, 0, render_x, height);
  line(0, render_y, width, render_y);
}

void keyPressed() {
  if (keyCode == LEFT) {
    shift_x += shift;
  }
  if (keyCode == RIGHT) {
    shift_x -= shift;
  }
  if (keyCode == UP) {
    shift_y += shift;
  }
  if (keyCode == DOWN) {
    shift_y -= shift;
  }
}

void mouseWheel(MouseEvent e) {
  float transformed_mouse_x = (mouseX * 1.0 / width) * zoom - zoom / 2 + shift_x;
  float transformed_mouse_y = -(mouseY * 1.0 / height) * zoom + zoom / 2 + shift_y;
  zoom *= 1 + 0.1 * e.getCount();
  shift_x = -(mouseX - width / 2.0) * (zoom / width) + transformed_mouse_x;
  shift_y = (mouseY - height / 2.0) * (zoom / height) + transformed_mouse_y;
}

void mouseDragged(MouseEvent e) {
  if (mouseButton == CENTER) {
    shift_x -= (mouseX - pmouseX) * 1.0 / width * zoom;
    shift_y += (mouseY - pmouseY) * 1.0 / height * zoom;
  }
}

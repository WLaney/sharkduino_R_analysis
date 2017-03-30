Table eas;
Integer sample;
Float roll;
Float pitch;
Float yaw;

void setup() {
  eas = loadTable("/Users/Centigonal/Sharkduino/sharkduino_R_analysis/data/3dcalibeuler.csv", "header");
  println(eas.getRowCount() + " total rows in table"); 
  
  sample = 1;
  
  frameRate(25);
  size(1024, 600, P3D); 
  rectMode(CENTER);
  camera(width*.75, height*0.2, (height/2.0) / tan(PI*30.0 / 180.0), width/2.0, height/2.0, 0, 0, 1, 0);
}

void draw() {
  //sample = int(map(mouseX, 0, width, 1, eas.getRowCount()));
  roll = eas.getFloat(sample, "roll");
  pitch = eas.getFloat(sample, "pitch");
  yaw = eas.getFloat(sample, "yaw");
  
  background(51); 
  lights();
  fill(150, 100, 200);
  stroke(255);
  
  pushMatrix();
  translate(width/2, height*0.4, 0); 
  rotateX(roll);
  rotateY(yaw);
  rotateZ(pitch);
  
  pushMatrix();
  translate(0, 0, -70); 
  box(150, 40, 220);
  popMatrix();
  
  translate(40, 0, 70); 
  box(70, 40, 57);
  popMatrix();
  
  
  //ground plane
  fill(200);
  stroke(255);
  
  translate(width/2, height*.6, 0); 
  rotateX(0);
  rotateY(0);
  rotateY(0); 
  box(500, 10, 500);
  
  sample = min(sample+1, eas.getRowCount()-1);
}
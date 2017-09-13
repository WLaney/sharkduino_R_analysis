Table eas;
Integer sample;
Float roll;
Float pitch;
Float yaw;

void setup() {
  size(1024, 600, P3D); 

  eas = loadTable("/Users/dara/Projects/Sharkduino/sharkduino_R_analysis/data/tmp-eas.csv", "header");
  println(eas.getRowCount() + " total rows in table"); 
  
  sample = 1;
  
  frameRate(25);
  rectMode(CENTER);
  camera(width*.75, -height/3, -height* tan(PI*20.0 / 180.0), width/2.0, height/2.0, 0, 0, 0, 1);
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
  translate(width/2, height*0.4, -75); 
  rotateY(yaw); 
  rotateZ(pitch);
  rotateX(roll);
  
  
  pushMatrix();
  translate(0, 70, 0); 
  box(150, 220, 40);
  popMatrix();
  
  translate(40, -70, 0); 
  box(70, 57, 40);
  popMatrix();
  
  
  //ground plane
  fill(200);
  stroke(255);
  
  translate(width/2, height*.5, height*.2); 
  rotateX(0);
  rotateY(0);
  rotateY(0); 
  box(500, 500, 10);
  
  sample = min(sample+1, eas.getRowCount()-1);
}
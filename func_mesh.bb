;####################################################################################################################
;#
;#  This is a collection of mesh handling functions
;#
;####################################################################################################################


;This function is used to merge all child entities back into the parent entity
;note: it is very buggy
Function merge_all_to_parent(ent)
   DestMesh=CreateMesh()
   RotateMesh ent,EntityPitch(ent,True),EntityYaw(ent,True),EntityRoll(ent,True)
   PositionMesh ent,EntityX(ent,True),EntityY(ent,True),EntityZ(ent,True)
   AddMesh ent,DestMesh

   If CountChildren(ent)>0 Then
      For s=1 To CountChildren(ent)
         MERGEMESH destmesh,GetChild(ent,s)
      Next
   EndIf

   Return destmesh
End Function


Function MERGEMESH(destmesh,ent)
   RotateMesh ent,EntityPitch(ent,True),EntityYaw(ent,True),EntityRoll(ent,True)
   PositionMesh ent,EntityX(ent,True),EntityY(ent,True),EntityZ(ent,True)
   AddMesh ent,destmesh
   If CountChildren(ent)>0 Then
      For s=1 To CountChildren(ent)
         MERGEMESH destmesh,GetChild(ent,s)
      Next
   EndIf
End Function


;####################################################################################################################

Function create_cube(segs=1,parent=0)
   mesh=CreateMesh( parent )
   For scnt=0 To 3
      surf=CreateSurface( mesh )
      stx#=-.5
      sty#=stx
      stp#=Float(1)/Float(segs)
      y#=sty
      For a=0 To segs
         x#=stx
         v#=a/Float(segs)
         For b=0 To segs
            u#=b/Float(segs)
            AddVertex(surf,x,y,0.5,u,v)
            x=x+stp
         Next
         y=y+stp
      Next
      For a=0 To segs-1
         For b=0 To segs-1
            v0 = a * (segs + 1) + b
            v1 = v0 + 1
            v2 = (a + 1) * (segs + 1) +  b + 1
            v3 = v2 - 1
            AddTriangle( surf, v0, v1, v2 )
            AddTriangle( surf, v0, v2, v3 )
         Next
      Next
      RotateMesh mesh,0,90,0
   Next
   ;top and bottom
   RotateMesh mesh,90,0,0
   For scnt=0 To 1
      surf=CreateSurface( mesh )
      stx#=-.5
      sty#=stx
      stp#=Float(1)/Float(segs)
      y#=sty
      For a=0 To segs
         x#=stx
         v#=a/Float(segs)
         For b=0 To segs
            u#=b/Float(segs)
            AddVertex(surf,x,y,0.5,u,v)
            x=x+stp
         Next
         y=y+stp
      Next
      For a=0 To segs-1
         For b=0 To segs-1
            v0=a*(segs+1)+b:v1=v0+1
            v2=(a+1)*(segs+1)+b+1:v3=v2-1
            AddTriangle( surf,v0,v1,v2 )
            AddTriangle( surf,v0,v2,v3 )
         Next
      Next
      RotateMesh mesh,180,0,0
   Next

   RotateMesh mesh,90,0,0
   ScaleMesh mesh,2,2,2
   UpdateNormals mesh
   Return mesh
End Function


;####################################################################################################################

Function create_flat(segs=1,parent=0)
   mesh=CreateMesh( parent )

   surf=CreateSurface( mesh )
   stx#=-.5
   sty#=stx
   stp#=Float(1)/Float(segs)
   y#=sty
   For a=0 To segs
      x#=stx
      v#=a/Float(segs)
      For b=0 To segs
         u#=b/Float(segs)
         AddVertex(surf,x,y,0.5,u,v)
         x=x+stp
      Next
      y=y+stp
   Next
   For a=0 To segs-1
      For b=0 To segs-1
         v0=a*(segs+1)+b:v1=v0+1
         v2=(a+1)*(segs+1)+b+1:v3=v2-1
         AddTriangle( surf,v0,v1,v2 )
         AddTriangle( surf,v0,v2,v3 )
      Next
   Next

   RotateMesh mesh,180,0,0
   ScaleMesh mesh,2,2,2
   EntityFX mesh,16
   UpdateNormals mesh
   Return mesh

End Function


;####################################################################################################################

Type f_torus
  Field x#
  Field y#
  Field z#
  Field v#
End Type

Function create_torus(seg=8,outerseg=16,parent=0,xloc#=0,yloc#=0,zloc#=0,rad1#=1,rad2#=3)
  ; seg# defines the number of vertices in the torus cross-section
  ; outerseg# defines the number of 'chambers' our torus will have about its circumference
  ; parent is the parent enitity handle
  ; xloc# is the final x axis location for the torus
  ; yloc# is the final y axis location for the torus
  ; zloc# is the final z axis location for the torus
  ; rad1# is the radius of the cross-section
  ; rad2# is the radius of the circumference

  If seg<3 Then seg=8 ; make sure the number of segments is set to something sane
  If seg>64 Then seg=64 ; change this if you need more segments
  If outerseg<3 Then outerseg=8 ; make sure the number of segments is set to something sane
  If outerseg>120 Then outerseg=120 ; change this if you need more segments

  torusmesh=CreateMesh(parent)
  torussurf=CreateSurface(torusmesh)
  
  angle#=0 ; Set our initial starting angle
  inc#=Float 360 / Float seg ; Setup increment for setting up vertices around our torus cross-section

  ; Do vertices
  For doverts = 0 To seg
    angle#=inc#*doverts
    verts.f_torus = New f_torus
    verts\x#=rad1#*Cos(angle#)
    verts\y#=rad1#*Sin(angle#)
    verts\z#=0
    verts\v#=angle#/360
    AddVertex (torussurf,verts\x#+rad2#,verts\y#,verts\z#,0,verts\v#)
  Next

  outside_inc#=Float 360 / Float outerseg ; Setup increment for setting up # of seg#ments that make up our torus sweep
  rotinc#=outside_inc#

  For rotseg= 1 To outerseg ; Rotate our initial verts around the Y axis
    For verts.f_torus = Each f_torus
      rx#=Cos(rotinc#)*(verts\x#+rad2#)+Sin(rotinc#)*verts\z#
      rz#=-Sin(rotinc#)*(verts\x#+rad2#)+Cos(rotinc#)*verts\z#
      u#=rotinc#/360
      AddVertex (torussurf,rx#,verts\y#,rz#,u#,verts\v#)
    Next
    rotinc#=Float(outside_inc#*(rotseg+1))
  Next

  ; Do sides of torus
  seginc=0
  For rottri = 1 To outerseg
    For vert= 0 To seg-1
      AddTriangle torussurf,vert+seginc,vert+seg+1+seginc,vert+seg+2+seginc
      AddTriangle torussurf,vert+seginc,vert+seg+2+seginc,vert+seginc+1
    Next
    seginc=(seg+1)*rottri
  Next

;  FlipMesh torusmesh ; Uncomment this is you want to put the camera inside the torus

  UpdateNormals torusmesh ; fix our normals

  MoveEntity torusmesh,xloc#,yloc#,zloc# ; Put the torus in place per passed X,Y,Z parameters

  For verts.f_torus = Each f_torus ; Clean up our data
    Delete verts
  Next

  Return torusmesh

End Function


;####################################################################################################################

; Tapered Cylinder function
; Written by Martin Parrott
; V1.1
; Oct. 28, 2001 - 1. Added ability to map texture on object
;                 2. Changed triangle creation order, caused by 1. above
;                 3. Cleaned up variable names
;
; This code is hiware. If you use it, please send me an email and say Hi!
; You are free to use, modify, etc. No warranty is written Or implied
; Use at your own risk
; This code is free to use, but if you modify it, please send the
; changes to the above email address so I can continue to release
; updates so others can benefit.

Function create_cylinder(seg=8,parent=0,solid=True,xloc#=0,yloc#=0,zloc#=0,rad1#=1,rad2#=.75,height#=2)
  ; seg defines the number of segments/vertices in the cylinder cross-section
  ; parent is the parent enitity handle
  ; solid defines whether the cylinder ends are capped or open, true-they are closed, false-they are open
  ; xloc# is the final x axis location for the cylinder
  ; yloc# is the final y axis location for the cylinder
  ; zloc# is the final z axis location for the cylinder
  ; rad1# is the radius of the bottom of the cylinder
  ; rad2# is the radius of the top of the cylinder
  ; height# is the total height of the cylinder

  If seg<2 Then seg=8 ; make sure the number of segments is set to something sane
  If seg>32 Then seg=32

  cylmesh=CreateMesh()
  cylsurf=CreateSurface(cylmesh)
  
  ;Create center vertex of bottom disc
  AddVertex cylsurf,0,0,0,1,1

  angle#=0 ; Set our initial starting angle
  inc#=Float 360 / Float seg ; Setup increment for setting up vertices around our cylinder ends

  ; Do bottom end vertices
  While angle# < 360.01
    x#=rad1#*Cos(angle#)
    z#=rad1#*Sin(angle#)
    u#=angle#/360
    AddVertex (cylsurf,x#,0,z#,u#,1)
    angle#=angle#+inc#
  Wend

  ; If solid is set, then cap end. Do triangles
  If solid>0
    For vert=1 To seg
      AddTriangle cylsurf,0,vert,vert+1
    Next
  EndIf

  ;Create center vertex of top disc
  AddVertex cylsurf,0,height#,0,0,0

  angle#=0 ; reset angle

  ; Do top end
  While angle# < 360.01
    x#=rad2#*Cos(angle#)
    z#=rad2#*Sin(angle#)
    u#=angle#/360
    AddVertex (cylsurf,x#,height#,z#,u#,0)
    angle#=angle#+inc#
  Wend

  ; If solid is set, then cap end. Do triangles
  If solid>0
    For vert=seg+3 To (seg*2)+3
      AddTriangle cylsurf,seg+2,vert+1,vert
    Next
  EndIf

  ; Do sides of cylinder
  For vert=1 To seg
    AddTriangle cylsurf,vert,vert+seg+2,vert+seg+3
    AddTriangle cylsurf,vert,vert+seg+3,vert+1
  Next

  UpdateNormals cylmesh ; fix our normals
  MoveEntity cylmesh,xloc#,yloc#,zloc# ; Put the cylinder in place

  If parent > 0 ; Assign our cylinder to a parent if one is passed to us
    EntityParent cylmesh,parent
  EndIf

  Return cylmesh

End Function


;####################################################################################################################

; Small_Branch_Size_Max#

; Trunk_seg This sets the number of faces in each trunk and branch cylinder
; LBranch_seg
; MBranch_seg
; SBranch_seg

; Trunk_Big_Dia# These set the bottom diameter of the trunk/branches
; Trunk_Small_Dia# These set the top diameter of the trunk/branches
; LBranch_Big_Dia#
; LBranch_Small_Dia#
; MBranch_Big_Dia#
; MBranch_Small_Dia#
; SBranch_Big_Dia#
; SBranch_Small_Dia#

; Texture section
; Uncomment the following and replace the image files with your own if you want
; textures on your trunk, branches and/or leaves
; Note: If you don't want these to be global variables, move them inside the
;       function below
;Global Trunk_texture$="trunk.bmp"
;Global LargeBranch_texture$="largebranch.bmp"
;Global MediumBranch_texture$="medbranch.bmp"
;Global SmallBranch_texture$="smallbranch.bmp"
;Global Leaf_texture$="leaf.bmp"

;Include "../lib/cylindertaper.bb" ; Needed to create the trunk and limbs of our tree
;Include "../lib/torus.bb" ; Needed for one of our strange leaf types!

Function create_tree(Wiggle_Flag=2,Branches_On_End_Flag=2,Leaf_Type=-1,Leaf_Mesh=100,Number_Of_Large_Branches=5,Number_Of_Medium_Branches=3,Number_Of_Small_Branches=7,Large_Branch_Minimum_Angle#=20,Medium_Branch_Minimum_Angle#=20,Small_Branch_Minimum_Angle#=20,Large_Branch_Maximum_Angle#=40,Medium_Branch_Maximum_Angle#=40,Small_Branch_Maximum_Angle#=40,Tree_Trunk_Size#=9,Large_Branch_Size_Min#=2,Medium_Branch_Size_Min#=2,Small_Branch_Size_Min#=2,Large_Branch_Size_Max#=4,Medium_Branch_Size_Max#=3,Small_Branch_Size_Max#=4,Trunk_seg=15,LBranch_seg=4,MBranch_seg=4,SBranch_seg=4,Trunk_Big_Dia#=1,Trunk_Small_Dia#=.8,LBranch_Big_Dia#=.6,LBranch_Small_Dia#=.4,MBranch_Big_Dia#=.3,MBranch_Small_Dia#=.2,SBranch_Big_Dia#=.1,SBranch_Small_Dia#=.05)

  ; System calculated variables. Takes from defaults above or passed function parameters

  L_Bmin#=Large_Branch_Minimum_Angle#
  L_Bmax#=Large_Branch_Maximum_Angle# - Large_Branch_Minimum_Angle#

  M_Bmin#=Medium_Branch_Minimum_Angle#
  M_Bmax#=Medium_Branch_Maximum_Angle# - Medium_Branch_Minimum_Angle#

  S_Bmin#=Small_Branch_Minimum_Angle#
  S_Bmax#=Small_Branch_Maximum_Angle# - Small_Branch_Minimum_Angle#

  Large_Branch_Size_Range#=Large_Branch_Size_Max# - Large_Branch_Size_Min#

  Medium_Branch_Size_Range#=Medium_Branch_Size_Max# - Medium_Branch_Size_Min#

  Small_Branch_Size_Range#=Small_Branch_Size_Max# - Small_Branch_Size_Min#

  If Trunk_texture$<>"" ; Get our Trunk texture if defined
    trunktex=LoadTexture(Trunk_texture$)
  Else
    trunktex=0
  EndIf

  If LargeBranch_texture$<>"" ; Get our Large branch texture if defined
    lbranchtex=LoadTexture(LargeBranch_texture$)
  Else
    lbranchtex=0
  EndIf

  If MediumBranch_texture$<>"" ; Get our Medium branch texture if defined
    mbranchtex=LoadTexture(MediumBranch_texture$)
  Else
    mbranchtex=0
  EndIf

  If SmallBranch_texture$<>"" ; Get our Small branch texture if defined
    sbranchtex=LoadTexture(SmallBranch_texture$)
  Else
    sbranchtex=0
  EndIf

  If Leaf_texture$<>"" ; Get our Leaf texture if defined
    leaftex=LoadTexture(Leaf_texture$,54)
  Else
    leaftex=0
  EndIf

  ; Make the Tree Trunk
  ; Note: the Make_Branch function puts a sphere on the end of the branch to 'smooth'
  ; up the construction. If you want to save polys, change all calls of Make_Branch
  ; below to Make_Branch_Nosphere

  Trunk=Make_Branch(Tree_Trunk_Size#,Trunk_Big_Dia#,Trunk_Small_Dia#,Trunk_seg,trunktex)

  A=0
  While A < Number_Of_Large_Branches
  
    ; Make one large branch.
  
    This_Large_Branch_Size#=( Rnd(0,1) * Large_Branch_Size_Range# ) + Large_Branch_Size_Min#
    LBranch=Make_Branch(This_Large_Branch_Size#,LBranch_Big_Dia#,LBranch_Small_Dia#,LBranch_seg,lbranchtex)

    If LargeBranch_texture$<>"" ; If the texture is defined, apply it
      EntityTexture LBranch,lbranchtex
    EndIf

    ; Loop To put medium branches on that large branch.
  
    B=0
    While B < Number_Of_Medium_Branches
  
      This_Medium_Branch_Size#=( Rnd(0,1) * Medium_Branch_Size_Range# ) + Medium_Branch_Size_Min#
    
      MBranch=Make_Branch(This_Medium_Branch_Size#,MBranch_Big_Dia#,MBranch_Small_Dia#,MBranch_seg,mbranchtex)
      EntityParent MBranch,LBranch

      If MediumBranch_texture$<>"" ; If the texture is defined, apply it
        EntityTexture MBranch,mbranchtex
      EndIf

      ; Loop To put small branches on that medium branch.
    
      C=0
      While C < Number_Of_Small_Branches
    
        This_Small_Branch_Size#=( Rnd(0,1) * Small_Branch_Size_Range# ) + Small_Branch_Size_Min#
      
        SBranch=Make_Branch_Nosphere(This_Small_Branch_Size#,SBranch_Big_Dia#,SBranch_Small_Dia#,SBranch_seg,sbranchtex)
        EntityParent SBranch,MBranch
      
        If SmallBranch_texture$<>"" ; If the texture is defined, apply it
          EntityTexture SBranch,sbranchtex
        EndIf

        ; The leaf at the End of the small branch.
    
        Temp_leaf=Make_Leaf(Leaf_Type,Leaf_Mesh,leaftex)
        PositionEntity Temp_leaf,0,This_Small_Branch_Size#,0

        ; First, spin the vertical branch To a random angle.
        ; The branch doesn't really change - this actually
        ; just spins the leaf around!
      
        SpinAngle=(Rnd(0,1)*360)
        EntityParent Temp_leaf,SBranch
        TurnEntity SBranch,0,spinangle,0,True
      
        If Leaf_texture$<>"" ; If the texture is defined, apply it
          EntityTexture Temp_leaf,leaftex
        EndIf

        ; Now, tilt it over a little.
        BranchAngle=(Rnd(0,1)*S_Bmax)+S_Bmin
        TurnEntity SBranch,0,0,BranchAngle,True

        ; Rotate it into place, with a little random wiggle.
        Wiggle=(Rnd(0,1)*20) - 10
        If Wiggle_Flag=2
          Wiggle=0
        EndIf
                
        C2=( 360 / Number_Of_Small_Branches ) * C
        C2=C2 + Wiggle
        TurnEntity SBranch,0,C2,0,True
      
        ; Move it up To the top of the Medium_Branch.
        ; If flag is "off", Then move To the Next spot on branch.
      
        TEMP_HEIGHT#=This_Medium_Branch_Size#;
    
        If ( Branches_On_End_Flag=2 )
           TEMP_HEIGHT#=This_Medium_Branch_Size# - ((This_Medium_Branch_Size# / Number_Of_Small_Branches) * C)
        EndIf
      
        PositionEntity SBranch,0,TEMP_HEIGHT#,0

        C=C + 1
      Wend

      BranchAngle=(Rnd(0,1)*M_Bmax)+M_Bmin
      TurnEntity MBranch,0,0,BranchAngle,True

      Wiggle=(Rnd(0,1)*20) - 10
      If ( Wiggle_Flag=2 )
        Wiggle=0
      EndIf
      B2=( 360 / Number_Of_Medium_Branches ) * B
      B2=B2 + Wiggle
      TurnEntity MBranch,0,B2,0,True
    
      ; Move the Medium_Branch up To the top of the Large_Branch.
      ; If flag is "off", Then move To the Next spot on branch.
              
      TEMP_HEIGHT#=This_Large_Branch_Size#
      If ( Branches_On_End_Flag=2 )
        TEMP_HEIGHT#=This_Large_Branch_Size# - ((This_Large_Branch_Size# / Number_Of_Medium_Branches)*B)
      EndIf
    
      PositionEntity MBranch,0,TEMP_HEIGHT#,0
      B=B + 1
    Wend

    BranchAngle=(Rnd(0,1)*L_Bmax)+L_Bmin
    TurnEntity LBranch,0,0,BranchAngle,True

    Wiggle=(Rnd(0,1)*20) - 10
    If Wiggle_Flag=2
      Wiggle=0
    EndIf

    A2=( 360 / Number_Of_Large_Branches ) * A
    A2=A2 + Wiggle
    TurnEntity LBranch,0,A2,0,True
    
    ; Move the Large_Branch up To the top of the Tree_Trunk.
    ; If flag is "off", Then move To the Next spot on trunk.
    ; These are spaced differently than the other branches - they
    ; start about 3/4 of the way up the trunk.
            
    TEMP_HEIGHT#=Tree_Trunk_Size#
    If Branches_On_End_Flag=2
      TEMP_HEIGHT#=Tree_Trunk_Size# - ((Tree_Trunk_Size# / Number_Of_Large_Branches) * A/4);
    EndIf
  
    PositionEntity LBranch,0,TEMP_HEIGHT#,0
    A=A + 1

    EntityParent LBranch,Trunk

  Wend

  Return Trunk

End Function

Function Make_Leaf(Leaf_Type,Leaf_Mesh,texture)

  Select Leaf_Type

    ; Let's do the quasi-REALISTIC leaf.
    Case 0

      piv0=CreatePivot()
      piv1=CreatePivot(piv0)

      leaf1=CreateSphere(8,piv1)
      EntityColor leaf1,0,150,0
      PositionEntity leaf1,0,-2.5,-5
      TurnEntity leaf1,-30,0,0,True
      ScaleEntity leaf1,2.4,.6,6

      piv2=CopyEntity(piv1,piv0)
      TurnEntity piv2,0,120,0

      piv3=CopyEntity(piv2,piv0)
      TurnEntity piv2,0,120,0

      ScaleEntity piv0,.1,.1,.1 ; Change this scale to size the overall cluster
      Return piv0

    ; Now create the actual Sphere Blob leaf.
    Case 1

      ball=CreateSphere(8)
      ScaleEntity ball,2,2,2 ; Change this to size the 'blobs' up and down
      EntityAlpha ball,.1
      EntityColor ball,0,150,0

      leafmesh=CreateMesh(ball)
      leafsurf=CreateSurface(leafmesh)

      A=1

      While A<=Leaf_Mesh

        ; Calculate random location For First point.
        X1#=( Rnd(0,1) * 2 ) - 1
        Y1#=( Rnd(0,1) * 2 ) - 1
        Z1#=( Rnd(0,1) * 2 ) - 1

        pnt0=AddVertex(leafsurf,x1#,y1#,z1#)
        pnt1=AddVertex(leafsurf,0,0,0)
        pnt2=AddVertex(leafsurf,0,0,.1)

        AddTriangle(leafsurf,pnt0,pnt1,pnt2)

        pnt1=AddVertex(leafsurf,0,0,0)
        pnt2=AddVertex(leafsurf,.1,0,0)

        AddTriangle(leafsurf,pnt0,pnt1,pnt2)

        A=A + 1
      
      Wend
      EntityColor leafmesh,0,150,0

      Return ball

    ; Now create the actual STRANGE leaf. Change this To anything!
    Case 2

      box=CreateCube()
      EntityColor box,0,150,0
      ScaleEntity box,2,2,2
      sph=CreateSphere(8,box)
      EntityColor sph,150,0,0
      ScaleEntity sph,1.2,1.2,1.2
      ScaleEntity box,.3,.3,.3
      Return box

    ; Now create the actual TORUS leaf.
    Case 3

      torus=create_torus(5,12,0,0,0,0,.25,1)
      EntityColor torus,150,0,0
      torcyl1=CreateCylinder(3,False,torus)
      EntityColor torcyl1,0,150,0
      ScaleEntity torcyl1,.1,1,.1
      TurnEntity torcyl1,90,0,0
      torcyl2=CreateCylinder(3,False,torus)
      EntityColor torcyl2,0,150,0
      ScaleEntity torcyl2,.1,1,.1
      TurnEntity torcyl2,0,0,90
      Return torus

    ; Create the Tri leaf Object - with lots of little triangles!!!
    Case 4

      leafmesh=CreateMesh()
      leafsurf=CreateSurface(leafmesh)
      A=1

      While A<=Leaf_Mesh
        ; Calculate random location For First point.
        X1#=( Rnd(0,1) * 2 ) - 1
        Y1#=( Rnd(0,1) * 2 ) - 1
        Z1#=( Rnd(0,1) * 2 ) - 1

        ;Move a little way from *First* point.
        X2#=X1# + ( Rnd(0,1) * 0.6 ) - 0.3
        Y2#=Y1# + ( Rnd(0,1) * 0.6 ) - 0.3
        Z2#=Z1# + ( Rnd(0,1) * 0.6 ) - 0.3

        ; Move a little way from *First* point.
        X3#=X1# + ( Rnd(0,1) * 0.6 ) - 0.3
        Y3#=Y1# + ( Rnd(0,1) * 0.6 ) - 0.3
        Z3#=Z1# + ( Rnd(0,1) * 0.6 ) - 0.3

        pnt0=AddVertex(leafsurf,X1#,Y1#,Z1#)
        pnt1=AddVertex(leafsurf,X2#,Y2#,Z2#)
        pnt2=AddVertex(leafsurf,X3#,Y3#,Z3#)

        AddTriangle(leafsurf,pnt0,pnt1,pnt2)

        A=A + 1
      
      Wend
      EntityColor leafmesh,0,150,0
      Return leafmesh

    ; Create the ULTRA-Tri leaf Object - with lots of little triangles,
    ; And Each "leaf" has a connector "branch" back To <0,0,0>.
    Case 5

      leafmesh=CreateMesh(parent)
      leafsurf=CreateSurface(leafmesh)

      A=1

      While A<=Leaf_Mesh

        ; Calculate random location For First point.
        X1=( Rnd(0,1) * 2 ) - 1
        Y1=( Rnd(0,1) * 2 ) - 1
        Z1=( Rnd(0,1) * 2 ) - 1

        ; Move a little way from *First* point.
        X2=X1 + ( Rnd(0,1) * 0.6 ) - 0.3
        Y2=Y1 + ( Rnd(0,1) * 0.6 ) - 0.3
        Z2=Z1 + ( Rnd(0,1) * 0.6 ) - 0.3

        ; Move a little way from *First* point.
        X3=X1 + ( Rnd(0,1) * 0.6 ) - 0.3
        Y3=Y1 + ( Rnd(0,1) * 0.6 ) - 0.3
        Z3=Z1 + ( Rnd(0,1) * 0.6 ) - 0.3

        pnt0=AddVertex(leafsurf,x1#,y1#,z1#)
        pnt1=AddVertex(leafsurf,x2#,y2#,z2#)
        pnt2=AddVertex(leafsurf,x3#,y3#,z3#)

        AddTriangle(leafsurf,pnt0,pnt1,pnt2)

        pnt1=AddVertex(leafsurf,0,0,0)
        pnt2=AddVertex(leafsurf,0,0,.1)

        AddTriangle(leafsurf,pnt0,pnt1,pnt2)

        pnt1=AddVertex(leafsurf,0,0,0)
        pnt2=AddVertex(leafsurf,.1,0,0)

        AddTriangle(leafsurf,pnt0,pnt1,pnt2)

        A=A + 1
      
      Wend
      EntityColor leafmesh,0,150,0
      Return leafmesh

    ; Map a user specified graphic on a flattened cube
    Case 6

      cube=CreateCube() ; Alternate to 'fast' leaf below. We map an image of a leaf on a flattened cube
      ScaleEntity cube,1,1,.001 ; Flatten our cube
      tex=LoadTexture("leaf.bmp",54) ; Change the image name to one of your own
      EntityTexture cube,tex
      Return cube

    ; Use FAST leaf as the Default.
    Default

      ball=CreateSphere(4)
      ScaleEntity ball,.3,.3,.3
      EntityColor ball,0,150,0
      Return ball

  End Select

End Function

Function Make_Branch(branch_size#,Big_Dia#,Small_Dia#,branch_seg,texture) ; Now create some branches.

  branch=create_cylinder(branch_seg,0,0,0,0,0,Big_Dia#,Small_Dia#,branch_size#)
  ball=CreateSphere(branch_seg/2,branch)
  ScaleEntity ball,Small_Dia#,Small_Dia#,Small_Dia#
  PositionEntity ball,0,branch_size,0

  If texture>0 ; If we have a texture, put it on
    EntityTexture branch,texture
    EntityTexture ball,texture
  Else ; If we don't have a texture, color it
    EntityColor branch,90,45,0 ; Give us a dark brown color, change to whatever color your want!
    EntityColor ball,90,45,0 ; Give us a dark brown color, change to whatever color your want!
  EndIf

  Return branch

End Function

Function Make_Branch_Nosphere(branch_size#,Big_Dia#,Small_Dia#,SBranch_seg,texture)

  branch=create_cylinder(SBranch_seg,0,0,0,0,0,Big_Dia#,Small_Dia#,branch_size#)

  If texture>0 ; If we have a texture, put it on
    EntityTexture branch,texture
  Else ; If we don't have a texture, color it
    EntityColor branch,90,45,0 ; Give us a dark brown color, change to whatever color your want!
  EndIf

  Return branch

End Function



;####################################################################################################################

Type Triangle							; This type is for the Remove_Coincident_Tris() function.
		Field Surface						; Pointer to the surface this triangle is in.
		Field Index							; The index number for this triangle in the specified surface.
End Type 	

; -------------------------------------------------------------------------------------------------------------------
; This function detects all front-to-front facing tris in a mesh, rebuilds the mesh without them, and returns a
; pointer to the new mesh.
;
; In other words, if two triangles face eachother in a mesh, and both triangles touch at 3 vertices, then both
; will be removed.  If the faces face in the same direction they will not be removed.
;
; Note that it is possible for some vertices to be left around orphaned with no triangle connected to them if all
; triangles that were connected to them were coincident with other triangles, but that shouldn't cause any problems,
; as they won't be visible.
; -------------------------------------------------------------------------------------------------------------------
Function Remove_Coincident_Tris(ThisMesh)
	; Find all the coincident tris.
		Surfaces = CountSurfaces(ThisMesh)

		; Loop through each surface of this mesh.
		For Surface_Index_1 = 1 To Surfaces
			Surface1 = GetSurface(ThisMesh, Surface_Index_1)
			Tris = CountTriangles(Surface1)

			; Loop through each triangle in this surface.
			For Tri_Index_1 = 0 To (Tris-1)
				; This triangle has not yet been found to be coincident with any other.
				Coincident = False
				
				; Loop through every triangle after this triangle in this surface.
				For Tri_Index_2 = (Tri_Index_1+1) To (Tris-1)
			
					; If these triangles are coincident...
					If TrisCoincident(Surface1, Tri_Index_1, Surface1, Tri_Index_2)
				
						; Mark this triangle as having been found to be coincident.
						Coincident = True
				
						; Mark both triangles for removal.
						ThisTriangle.Triangle = New Triangle
						ThisTriangle\Surface = Surface1
						ThisTriangle\Index 	 = Tri_Index_1
					
						ThisTriangle.Triangle = New Triangle
						ThisTriangle\Surface = Surface1
						ThisTriangle\Index 	 = Tri_Index_2
	
						; Exit the Tri_Index_2 loop.
						Exit
					
					EndIf
						
				Next

				; If we found the first triangle to be coincident with another already, don't look for any more
				; triangles coincident with this one.
				If Not Coincident
					; Otherwise, we failed to find a coincident triangle in the same surface as the triangle we're
					; testing, so look in all the other surfaces.

					; Loop through every triangle in every surface after this surface.
					For Surface_Index_2 = Surface_Index_1+1 To Surfaces
						Surface2 = GetSurface(ThisMesh, Surface_Index_2)
						Tris2 = CountTriangles(Surface2)

						For Tri_Index_2 = 0 To Tris2-1
						
							; If these triangles are coincident...
							If TrisCoincident(Surface1, Tri_Index_1, Surface2, Tri_Index_2)
								; Mark this triangle as having been found to be coincident.
								Coincident = True
				
								; Mark both triangles for removal.
								ThisTriangle.Triangle = New Triangle
								ThisTriangle\Surface = Surface1
								ThisTriangle\Index 	 = Tri_Index_1
					
								ThisTriangle.Triangle = New Triangle
								ThisTriangle\Surface = Surface2
								ThisTriangle\Index 	 = Tri_Index_2
	
								; Exit the Tri_Index_2 loop.
								Exit
					
							EndIf

						Next						

						; If we found the first triangle to be coincident with another already, exit Surface_Index_2 loop.
						If Coincident Then Exit
		
					Next

				EndIf

			Next 
		
		Next
	
	
	; Delete all the coincident tris by constructing a new mesh without those tris from the old mesh.

		; Create a new mesh.
		NewMesh = CreateMesh()	

		; Loop through each surface of the mesh.
		For Surface_Index = 1 To Surfaces

			; Get the pointer to the this surface and the number of vertices in it.
			SrcSurface = GetSurface(ThisMesh, Surface_Index)
			Tris = CountTriangles(SrcSurface)
	
			; Create a new surface in the destination mesh to hold the copy of this surface's data.
			DestSurface = CreateSurface(NewMesh)
			
			; Copy all the vertices from the source surface to the destination surface.
			SrcVerts = CountVertices(SrcSurface)
			For VertLoop = 0 To SrcVerts-1
		
				Vx#  = VertexX#(SrcSurface, VertLoop)
				Vy#  = VertexY#(SrcSurface, VertLoop)
				Vz#  = VertexZ#(SrcSurface, VertLoop)
				Vu#  = VertexU#(SrcSurface, VertLoop)
				Vv#  = VertexV#(SrcSurface, VertLoop)		
				Vw#  = VertexW#(SrcSurface, VertLoop)
				Vnx# = VertexNX#(SrcSurface, VertLoop)
				Vny# = VertexNY#(SrcSurface, VertLoop)
				Vnz# = VertexNZ#(SrcSurface, VertLoop)						
				Vr   = VertexRed(SrcSurface, VertLoop)
				Vg   = VertexGreen(SrcSurface, VertLoop)
				Vb   = VertexBlue(SrcSurface, VertLoop)
				AddVertex(DestSurface, Vx#, Vy#, Vz#, Vu#, Vv#, Vw#)
				VertexNormal(DestSurface, VertLoop, Vnx#, Vny#, Vnz#)
				VertexColor(DestSurface, VertLoop, Vr, Vg, Vb) 
	
			Next

			; Copy all triangles from the source surface to the destination surface.	
			SrcTris  = CountTriangles(SrcSurface)
			For TriLoop = 0 To SrcTris-1
	
				Copy_Tri = True
				
				For ThisTri.Triangle = Each Triangle
									
					; If this triangle is a coincident triangle that should be removed...
					If (ThisTri\Surface = SrcSurface) And (ThisTri\Index = TriLoop) 
						
						; Do not copy the triangle.
						Copy_Tri = False
						
						; Exit ThisTri loop early.
						Exit
						
					EndIf
						
				Next
	
	
				; If it's okay to copy this triangle...
				If Copy_Tri			
		
					V0 = TriangleVertex(SrcSurface, TriLoop, 0)
					V1 = TriangleVertex(SrcSurface, TriLoop, 1)
					V2 = TriangleVertex(SrcSurface, TriLoop, 2)
					AddTriangle(DestSurface, V0, V1, V2)
		
				EndIf
	
			Next
		
		Next


	; Delete the old mesh.
	FreeEntity ThisMesh
		
	; Delete all the temporary coincident triangle data.		
	Delete Each Triangle

	; Return the pointer to the new mesh.
	Return NewMesh
	
End Function 


; -------------------------------------------------------------------------------------------------------------------
; This function returns the squared distance between two vertices.
; -------------------------------------------------------------------------------------------------------------------
Function VertexDist#(Surface1, Vert1, Surface2, Vert2)

	V1x# = VertexX#(Surface1, Vert1)
	V1y# = VertexY#(Surface1, Vert1)
	V1z# = VertexZ#(Surface1, Vert1)
	
	V2x# = VertexX#(Surface2, Vert2)
	V2y# = VertexY#(Surface2, Vert2)
	V2z# = VertexZ#(Surface2, Vert2)
	
	Return (V1x#-V2x#)*(V1x#-V2x#) + (V1y#-V2y#)*(V1y#-V2y#) + (V1z#-V2z#)*(V1z#-V2z#)	
	
End Function	


; -------------------------------------------------------------------------------------------------------------------
; This function returns true if two triangles are coincident.  (Occupy the same space.)
;
; Epsilon is the distance by which the vertices of the triangles can be seperated and still be considered coincident.
; -------------------------------------------------------------------------------------------------------------------
Function TrisCoincident(Surface1, Tri_Index_1, Surface2, Tri_Index_2, Epsilon#=0.001)

	; Square the epsilon so that we can use squared distances for speed in comparisons.	
	Epsilon# = Epsilon#*Epsilon# 	
		
	; Store the indices of the vertices which make up the triangles.
	T1_Vert0 = TriangleVertex(Surface1, Tri_Index_1, 0)		
	T1_Vert1 = TriangleVertex(Surface1, Tri_Index_1, 1)
	T1_Vert2 = TriangleVertex(Surface1, Tri_Index_1, 2)
			
	T2_Vert0 = TriangleVertex(Surface2, Tri_Index_2, 0)		
	T2_Vert1 = TriangleVertex(Surface2, Tri_Index_2, 1)
	T2_Vert2 = TriangleVertex(Surface2, Tri_Index_2, 2)

	; Check to see if all three vertices of these triangles are coincident.

		Coincident = True
			
		; Check to see if vertex 0 of triangle 1 is coincident with any of the vertices in triangle 2.
		If VertexDist#(Surface1, T1_Vert0, Surface2, T2_Vert0) > Epsilon#
			If VertexDist#(Surface1, T1_Vert0, Surface2, T2_Vert1) > Epsilon#
				If VertexDist#(Surface1, T1_Vert0, Surface2, T2_Vert2) > Epsilon#
					Coincident = False
				EndIf
			EndIf
		EndIf
		
		; If the first test passed...
		If Coincident
				
			; Check to see if vertex 1 of triangle 1 is coincident with any of the vertices in triangle 2.
			If VertexDist#(Surface1, T1_Vert1, Surface2, T2_Vert0) > Epsilon#
				If VertexDist#(Surface1, T1_Vert1, Surface2, T2_Vert1) > Epsilon#
					If VertexDist#(Surface1, T1_Vert1, Surface2, T2_Vert2) > Epsilon#
						Coincident = False
					EndIf
				EndIf
			EndIf

			; If the second test passed...						
			If Coincident
					
				; Check to see if vertex 2 of triangle 1 is coincident with any of the vertices in triangle 2.
				If VertexDist#(Surface1, T1_Vert2, Surface2, T2_Vert0) > Epsilon#
					If VertexDist#(Surface1, T1_Vert2, Surface2, T2_Vert1) > Epsilon#
						If VertexDist#(Surface1, T1_Vert2, Surface2, T2_Vert2) > Epsilon#
							Coincident = False
						EndIf
					EndIf
				EndIf
				
			EndIf
														
		EndIf	
	
	; Return whether the triangles were coincident or not.
	Return Coincident
			
End Function


;####################################################################################################################
;                                                    
; Autosmooth by Peter Schuetz                        
;                                                    
; Function to give a mesh hard/soft edges            
; Welds/explodes vertices                            
;                                                    
; Works on single meshes
; Check for children before calling!
;                                                    
; Version 1.0, 18 Martch 2002                        
; Rev 1        30 Martch 2002
; Changed defaults and description 
; Rev 2        5 April 2002
; Removed obsolete params in Showmodel() 
;                                                    
; Public Domain, but try to give credit if used :)   
;                                                    
;------------------------------------------------------------

;Bank offsets for Vertices
Const VertStructSize=80

Const C_X=0
Const C_Y=4
Const C_Z=8

Const C_NX=12
Const c_NY=16
Const C_NZ=20

Const C_R=24
Const C_G=28
Const C_B=32

Const C_U=36
Const C_V=40
Const C_W=44

Const C_AvgNX=48
Const C_AvgNY=52
Const C_AvgNZ=56

Const C_ISUSED=60
Const C_NewIndex=64
Const C_Qx=68
Const C_PREV=72
Const C_NEXT=76


;Bank offsets for Triangles
Const TriStructSize=12

Const C_V1=0
Const C_V2=4
Const C_V3=8




; Parameters
;
; m = The Mesh to smooth
; sin_angle = The angle below witch triangles are to be smooth shaded
; If sin_angle is less than -1, all smooth everything welded 
; If sin_angle is larger than 1, all flat, every triangle has its own set of vertices
; vThresh = weld distancee for vertices (Box compare)
; tThresh = weld distancee for texture coords (Box compare)
; cThresh = weld distancee for color values (Not tested, commented out)


Function AutoSmooth(m,sin_angle#,vThresh#=.00001,tThresh#=.00001,cThresh#=5)

	Local maxVcount=0
	Local maxTcount=0
	;Local vThresh#=0.00001  ; ajust this if you poly collapses or does not weld
	;Local tThresh#=0.000001
	;Local cThresh#=.1	; not testet!
		
	Local sc,k,vc,tc,b,i,v,t,v1,v2,v3,vOff,tOff

	Local surf
	Local B1,B2,B3,B4
	
	Local qGrups
	Local minX#, xRange#,maxX#, xScalFactor#
	Local vprev,vnext
	
	Local Ax#,Ay#,Az#,Bx#,By#,Bz#,Cx#,Cy#,Cz#,vLen#   
	
	Local vx#,vy#,vz#,tU#,tV#,tW#,vR#,vG#,vB#
	Local nx1#,ny1#,nz1#,nx2#,ny2#,nz2#
	
	Local OrgVOff1,OrgVOff2,OrgVOff3
	
	;calculate max size of banks
	sc=CountSurfaces(m)
	For k=1 To sc
		surf=GetSurface( m,k )
		vc=CountVertices(surf)
		tc=CountTriangles(surf)
			If vc>maxVcount  maxVcount=vc
			If tc>maxTcount maxTcount=tc
		Next 
	
	
		qGroups=Int(maxVcount/10)	; maybe try different values for qGroups
		If qGroups<1 Then qGroups=1
	
	
		;allocate banks for the largest vertice and triangle counts
		B1=CreateBank (maxVcount*VertStructSize) 	; bank for original vertices
		B2=CreateBank (maxTcount*TriStructSize) 	; bank for triangles
		B3=CreateBank (maxTcount*VertStructSize*3) 	; new seperated vertices
 		B4=CreateBank (qGroups*4) 					;entry point for group
	
	
	For k=1 To sc
		surf=GetSurface( m,k )
		vc=CountVertices(surf)
		tc=CountTriangles(surf)

		maxX# = -10000000000000000
		minX# = 10000000000000000
		
		For v=0 To vc -1
			b=v*VertStructSize
			
			x#=VertexX(surf,v)
			PokeFloat B1,b,VertexX(surf,v)
			PokeFloat B1,b+C_Y,VertexY(surf,v)
			PokeFloat B1,b+C_Z,VertexZ(surf,v)
			
			If x#> maxX# Then maxX#=x#
			If x#< minX# Then minX#=x#
			
			; making new!
			;PokeFloat B1,b+C_NX,VertexNX(surf,v)
			;PokeFloat B1,b+C_NY,VertexNY(surf,v)
			;PokeFloat B1,b+C_NZ,VertexNZ(surf,v)


			PokeFloat B1,b+C_R,VertexRed(surf,v)
			PokeFloat B1,b+C_G,VertexGreen(surf,v)
			PokeFloat B1,b+C_B,VertexBlue(surf,v)


			PokeFloat B1,b+C_U,VertexU(surf,v)
			PokeFloat B1,b+C_V,VertexV(surf,v)
			PokeFloat B1,b+C_W,VertexW(surf,v)

			

		Next
			
			; *** calc quant qx

			xRange#=maxX#-minX#
			xScalFactor#=xRange#/(qGroups-1)
			
			For v=0 To vc-1
				b=v*VertStructSize
				PokeInt B1,b+C_Qx,Int((PeekFloat( B1,b+C_X)-minX#)/xScalFactor#)
			Next

		
		
			; init groups index
			For i=0 To qGroups -1
				PokeInt B4,i*4,-1
			Next
		
		
		
		; create new vertices for all triange corners		
		For t=0 To tc -1
			b=t*TriStructSize
			
			;offset in new vertice list:
			voff=t*3*VertStructSize
			
			v1=voff
			v2=voff+1*VertStructSize
			v3=voff+2*VertStructSize
	
	
			; now copy original vertice data:
			OrgVOff1=TriangleVertex(surf,t,0)*VertStructSize
			CopyBank B1,OrgVOff1,B3,v1,VertStructSize
	
			OrgVOff2=TriangleVertex(surf,t,1)*VertStructSize
			CopyBank B1,OrgVOff2,B3,v2,VertStructSize		
		
			OrgVOff3=TriangleVertex(surf,t,2)*VertStructSize
			CopyBank B1,OrgVOff3,B3,v3,VertStructSize


			; new vertice offset in bank 3
			PokeInt B2,b+C_V1,v1	; no need to read original 
			PokeInt B2,b+C_V2,v2	; cause new verts are created for all tri-corners
			PokeInt B2,b+C_V3,v3	


			vx1#=PeekFloat (B3,v1+C_X)
			vy1#=PeekFloat (B3,v1+C_Y)
			vz1#=PeekFloat (B3,v1+C_Z)
			
			vx2#=PeekFloat (B3,v2+C_X)
			vy2#=PeekFloat (B3,v2+C_Y)
			vz2#=PeekFloat (B3,v2+C_Z)
						
			vx3#=PeekFloat (B3,v3+C_X)
			vy3#=PeekFloat (B3,v3+C_Y)
			vz3#=PeekFloat (B3,v3+C_Z)
			
			
		;Add a normal
			
			;get two vectors

			Ax#=vx2#-vx1#
			Ay#=vy2#-vy1#
			Az#=vz2#-vz1#
	
			Bx#=vx3#-vx1#
			By#=vy3#-vy1#
			Bz#=vz3#-vz1#
	

			;crossproduct
			Cx# = Ay# * Bz# - By# * Az#
    		Cy# = Az# * Bx# - Bz# * Ax#
    		Cz# = Ax# * By# - Bx# * Ay#


			;normalize:
			vLen#=Sqr#((Cx#*Cx#)+(Cy#*Cy#)+(Cz#*Cz#))
			
			Cx#=Cx#/vLen#
			Cy#=Cy#/vLen#
			Cz#=Cz#/vLen#
			
		; copy face normal to all 3 vertices
			PokeFloat B3,v1+C_NX,Cx#
			PokeFloat B3,v1+C_NY,Cy#
			PokeFloat B3,v1+C_NZ,Cz#
	
			PokeFloat B3,v2+C_NX,Cx#
			PokeFloat B3,v2+C_NY,Cy#
			PokeFloat B3,v2+C_NZ,Cz#	

			PokeFloat B3,v3+C_NX,Cx#
			PokeFloat B3,v3+C_NY,Cy#
			PokeFloat B3,v3+C_NZ,Cz#	
			

		;init the average with face normal as shared normals will accumulate here
;			PokeFloat B3,v1+C_AvgNX,Cx#
;			PokeFloat B3,v1+C_AvgNY,Cy#
;			PokeFloat B3,v1+C_AvgNZ,Cz#
	
;			PokeFloat B3,v2+C_AvgNX,Cx#
;			PokeFloat B3,v2+C_AvgNY,Cy#
;			PokeFloat B3,v2+C_AvgNZ,Cz#	

;			PokeFloat B3,v3+C_AvgNX,Cx#
;			PokeFloat B3,v3+C_AvgNY,Cy#
;			PokeFloat B3,v3+C_AvgNZ,Cz#

			;faster?	
			CopyBank B3,v1+C_NX,B3,v1+C_AvgNX,12
			CopyBank B3,v2+C_NX,B3,v2+C_AvgNX,12
			CopyBank B3,v3+C_NX,B3,v3+C_AvgNX,12	
						
		Next ; t

				
		ClearSurface surf,True,True

		
		;New vertexcount
		vc=3*tc 
		
; Groups vertices into qGroups groups, based on quantizised x coord
; - allows for faster processing
; B3 doubles as qGroups linked lists.		

		
		; mark all new vertices in use, and make group lists:
		For v=0 To vc-1
			b=v*VertStructSize
			; mark all new vertices used
			PokeInt B3,v*VertStructSize+C_ISUSED,True
			
			
			;make grup lists:
			vxgrp=	PeekInt (B3,b+C_Qx)
			If PeekInt(B4,vxgrp*4)=-1 Then 	; is this the first in the group?
				PokeInt B4,vxgrp*4,b 		; store offset to first vertice in group
				PokeInt B3,b+C_PREV,-1 		; mark start of list
				PokeInt B3,b+C_NEXT,-1 		; mark end of list
			
			Else
				vgl=PeekInt (B4,vxgrp*4)  	;read offset to last entry in list
				
				PokeInt B3,vgl+C_NEXT,b		;make v1 the next
				PokeInt B3,b+C_PREV,vgl 	
				PokeInt B3,b+C_NEXT,-1		; mark end of list
				PokeInt B4,vxgrp*4,b		;store offset to *last* vertice in group

			EndIf
		Next 




;process surface		

	If sin_angle#<= 1 Then ; collapse vertices
	
		
		
		
		
		For g=0 To qGroups-1
			vcmp=PeekInt(B4,g*4)	; get list entry (pointing to last entry )

			

			; replace with :
			While vcmp<>-1 						;is group empty?
				vx#=PeekFloat (B3,vcmp+C_X)
				vy#=PeekFloat (B3,vcmp+C_Y)
				vz#=PeekFloat (B3,vcmp+C_Z)
				
				nx1#=PeekFloat (B3,vcmp+C_NX)
				ny1#=PeekFloat (B3,vcmp+C_NY)
				nz1#=PeekFloat (B3,vcmp+C_NZ)	
	
	
				tU#=PeekFloat (B3,vcmp+C_U)
				tV#=PeekFloat (B3,vcmp+C_V)
				tW#=PeekFloat (B3,vcmp+C_W)	
				
				cR#=PeekFloat (B3,vcmp+C_R)
				cG#=PeekFloat (B3,vcmp+C_G)
				cB#=PeekFloat (B3,vcmp+C_B)		

			
			v=PeekInt(B3,vcmp+C_PREV)			; now in the list look up previus
			
			;walk list (list is walked from bottom to top, so vertices will come out in reverse order)
			While v<>-1	; 					
				vprev=PeekInt(B3,v+C_PREV)				
				vnext=PeekInt(B3,v+C_NEXT)
				

				If Abs(vx# - PeekFloat (B3,v+C_X))<=vThresh# Then
				If Abs(vy# - PeekFloat (B3,v+C_Y))<=vThresh#  Then
				If Abs(vz# - PeekFloat (B3,v+C_Z))<=vThresh#  Then
			; comment if you dont want texture coord checking
				If Abs(tU# - PeekFloat (B3,v+C_U))<=tThresh#  Then
				If Abs(tV# - PeekFloat (B3,v+C_V))<=tThresh#  Then
				If Abs(tW# - PeekFloat (B3,v+C_W))<=tThresh#  Then
				
			; uncomment if you want vertex color checking	
			;	If Abs(cR# - PeekFloat (B3,v+C_R))<=cTresh#  Then
			;	If Abs(cG# - PeekFloat (B3,v+C_G))<=cTresh#  Then
			;	If Abs(cB# - PeekFloat (B3,v+C_B))<=cTresh#  Then
									
			 		nx2#=PeekFloat (B3,v+C_NX)
					ny2#=PeekFloat (B3,v+C_NY)
					nz2#=PeekFloat (B3,v+C_NZ)							
					
					DotProduct#=nx1#*nx2#+ny1#*ny2#+nz1#*nz2#
					
					; compare smoothing angle aginst face normal
					If DotProduct#>sin_Angle#  Then 
						;maching vertex found, mark this one not used
						PokeInt B3,v+C_ISUSED,False
						
						
						i=Int(v/VertStructSize)
						tOff=Int(i/3)
						corner=(i Mod 3)*4
							
						; point triangle at the found vertex
						PokeInt B2,tOff*TriStructSize+C_V1+corner,vcmp
					
					
						; add to the averaged normal						
					
						PokeFloat B3,vcmp+C_AvgNX, PeekFloat (B3,vcmp+C_AvgNX)+PeekFloat (B3,v+C_NX)
						PokeFloat B3,vcmp+C_AvgNY, PeekFloat (B3,vcmp+C_AvgNY)+PeekFloat (B3,v+C_NY)
						PokeFloat B3,vcmp+C_AvgNZ, PeekFloat (B3,vcmp+C_AvgNZ)+PeekFloat (B3,v+C_NZ)
						

						;remove	vertex from list
						If vprev<>-1 Then
							PokeInt B3,vprev+C_NEXT,vnext ; copy next offset (could be -1 )
						EndIf

						If vnext<>-1 Then
							PokeInt B3,vnext+C_PREV,vprev ; copy offset  (could be -1 )
						EndIf
						
					
					EndIf
					
		; uncomment if you want vertex color checking
		;		EndIf
		;		EndIf
		;		EndIf
		
		; comment if you dont want texture coord checking					
				EndIf
				EndIf
				EndIf					
				
				EndIf
				EndIf
				EndIf
			
				v=vprev
			Wend


			PokeInt B4,g*4,PeekInt(B3,vcmp+C_PREV)
			vcmp=PeekInt(B4,g*4)	; get list entry (pointing to last entry )
			Wend 	
		
		Next 

        EndIf



		; calc the vertex new index
		newIndex=0
		For v=0 To vc-1
			; been using offsets, now reverting to index
			PokeInt B3,v*VertStructSize+C_NewIndex,newIndex
			If PeekInt(B3,v*VertStructSize+C_ISUSED) Then newIndex=newIndex+1
						
		Next

	;Create new surface	
	;**************************
		
		For v=0 To vc -1
			b=v*VertStructSize


			If PeekInt( B3,b+C_ISUSED)=True Then

				newV=AddVertex( surf,PeekFloat (B3,b),PeekFloat (B3,b+C_Y),PeekFloat (B3,b+C_Z), PeekFloat (B3,b+C_U),PeekFloat (B3,b+C_V),PeekFloat(B3,b+C_W))
	
		;		normalize :	
				Cx#=PeekFloat (B3,b+C_AvgNX)
				Cy#=PeekFloat (B3,b+C_AvgNY)
				Cz#=PeekFloat (B3,b+C_AvgNZ)
				vLen#=Sqr#((Cx#*Cx#)+(Cy#*Cy#)+(Cz#*Cz#))

			
				Cx#=Cx#/vLen#
				Cy#=Cy#/vLen#
				Cz#=Cz#/vLen#

				VertexNormal surf,newV,Cx#,Cy#,Cz#		
				VertexColor surf,newV,PeekFloat (B3,b+C_R),PeekFloat (B3,b+C_G),PeekFloat (B3,b+C_B)
			End If

		Next
	
		;add the triangles
		For t=0 To tc -1
			b=t*TriStructSize
			; been using offsets, now reverting to index
			; look up new index from original:
			newV1=PeekInt(B3,PeekInt(B2,b+C_V1)+C_NewIndex)
			newV2=PeekInt(B3,PeekInt(B2,b+C_V2)+C_NewIndex)
			newV3=PeekInt(B3,PeekInt(B2,b+C_V3)+C_NewIndex)
			AddTriangle surf,newV1,newV2,newV3		
		Next	
		

	Next
	
	FreeBank B1
	FreeBank B2
	FreeBank B3	
	FreeBank B4
	

End Function


;####################################################################################################################
;
;initial coding done by:  Chris Chadwick 2003 
;
;This is some basic steps on how to convert regular objects to explodable ones...
;
;   shape.explode_ctrlT = copy_mesh_explode(pyramid,150,3,0, 1) 
;   FreeEntity pyramid 
;   PositionEntity shape\mesh,-4,3,i 
;   shape\exploding = True
;   EntityFX shape\mesh,16  ; (this disables backface culling, and looks better without culling most the time)


Type tri_linkT 
   Field prev.tri_linkT 
   Field surf 
   Field tri 
   Field dx#,dy#,dz# 
   Field pitch#,yaw#,roll# 
End Type 

Type explode_ctrlT 
   Field tri_list.tri_linkT 
   Field mesh 
   Field life% 
   Field fader# 
   Field fade_start% 
   Field exploding%
   Field rotate%
End Type 


;--------------------------------------------------------------
; 
; Updates ALL explodable meshes that are currently exploding. 
; 
;this must be called for every frame when exploding items are active:
;
;   update_mesh_explode()
;
Function update_mesh_explode() 
   For this.explode_ctrlT = Each explode_ctrlT 
      If this\exploding 
         If this\life
            tri_mesh = CreateMesh() 
            tri_surf = CreateSurface(tri_mesh) 
            AddVertex(tri_surf,0,0,0)
            AddVertex(tri_surf,0,0,0)
            AddVertex(tri_surf,0,0,0)
            AddTriangle(tri_surf,0,1,2)
;            HideEntity tri_mesh 

            tri.tri_linkT = this\tri_list 

            If this\life <= this\fade_start 
               EntityAlpha this\mesh,this\life * this\fader 
            EndIf 

            ; Update all tris in linked list. 
            While tri <> Null 
               v0 = TriangleVertex(tri\surf,tri\tri,0) 
               v1 = TriangleVertex(tri\surf,tri\tri,1) 
               v2 = TriangleVertex(tri\surf,tri\tri,2) 

               x0# = VertexX(tri\surf,v0) 
               y0# = VertexY(tri\surf,v0) 
               z0# = VertexZ(tri\surf,v0) 
               x1# = VertexX(tri\surf,v1) 
               y1# = VertexY(tri\surf,v1) 
               z1# = VertexZ(tri\surf,v1) 
               x2# = VertexX(tri\surf,v2) 
               y2# = VertexY(tri\surf,v2) 
               z2# = VertexZ(tri\surf,v2) 
               cx# = VertexX(tri\surf,v2+1) 
               cy# = VertexY(tri\surf,v2+1) 
               cz# = VertexZ(tri\surf,v2+1) 

               ; Load scratch pad tri mesh. 
               VertexCoords tri_surf,0,x0-cx,y0-cy,z0-cz 
               VertexCoords tri_surf,1,x1-cx,y1-cy,z1-cz 
               VertexCoords tri_surf,2,x2-cx,y2-cy,z2-cz 
               VertexNormal tri_surf,0,VertexNX(tri\surf,v0),VertexNY(tri\surf,v0),VertexNZ(tri\surf,v0) 
               VertexNormal tri_surf,1,VertexNX(tri\surf,v1),VertexNY(tri\surf,v1),VertexNZ(tri\surf,v1) 
               VertexNormal tri_surf,2,VertexNX(tri\surf,v2),VertexNY(tri\surf,v2),VertexNZ(tri\surf,v2) 

               ; Do rotations. 
               If this\rotate Then RotateMesh tri_mesh,tri\pitch,tri\yaw,tri\roll 

               ; Copy back to mesh tri. 
               x0# = VertexX(tri_surf,0) 
               y0# = VertexY(tri_surf,0) 
               z0# = VertexZ(tri_surf,0) 
               x1# = VertexX(tri_surf,1) 
               y1# = VertexY(tri_surf,1) 
               z1# = VertexZ(tri_surf,1) 
               x2# = VertexX(tri_surf,2) 
               y2# = VertexY(tri_surf,2) 
               z2# = VertexZ(tri_surf,2) 

               VertexCoords tri\surf,v0,x0+cx+tri\dx,y0+cy+tri\dy,z0+cz+tri\dz 
               VertexCoords tri\surf,v1,x1+cx+tri\dx,y1+cy+tri\dy,z1+cz+tri\dz 
               VertexCoords tri\surf,v2,x2+cx+tri\dx,y2+cy+tri\dy,z2+cz+tri\dz 
               VertexNormal tri\surf,v0,VertexNX(tri_surf,0),VertexNY(tri_surf,0),VertexNZ(tri_surf,0) 
               VertexNormal tri\surf,v1,VertexNX(tri_surf,1),VertexNY(tri_surf,1),VertexNZ(tri_surf,1) 
               VertexNormal tri\surf,v2,VertexNX(tri_surf,2),VertexNY(tri_surf,2),VertexNZ(tri_surf,2) 

               VertexCoords tri\surf,v2+1,cx+tri\dx,cy+tri\dy,cz+tri\dz 
              ; tri\dy = tri\dy - GRAVITY#

               tri = tri\prev 
            Wend 

            FreeEntity (tri_mesh)

            this\life = this\life - 1 
         Else 
            ; Mesh has finished exploding. 
            free_mesh_explode(this) 
         EndIf 
      EndIf 

   Next 

End Function 


; 
; Creates a subdivided (optional), unwelded copy of an existing mesh. 
; Explode info for all tris in the new mesh are kept in a separate linked list 
; contained in the control variable returned to the caller. 
; Note: the source mesh being copied remains completely unaltered. 
; 
; Params: 
; s_mesh - Source mesh to be copied. 
; life - Duration of explosion animation (in frames) to use when exploded. 
; divs - Number of times to perform x4 sub-division on each source tri. 
; keep_surfs - True to keep the same amount of surfaces in 
; the copy as in the source mesh. 
; False (Default) to copy all tris in the source mesh 
; to a single surface in the copy. 
; 
; Returns: 
; Pointer to the control variable (of type explode_ctrlT) for the 
; new explodable mesh created. 
; 
Function copy_mesh_explode.explode_ctrlT(s_mesh, life% = 150, divs = 0, keep_surfs% = False, rot% = True, FADE_START# = 0.5) 
   tri_list.tri_linkT = Null 
   d_mesh = CreateMesh() 

   For sno = 1 To CountSurfaces(s_mesh) 
      s_surf = GetSurface(s_mesh,sno) 
      nt = CountTriangles(s_surf) 

      If sno = 1 Or keep_surfs Then d_surf = CreateSurface(d_mesh) 
      For tno = 0 To nt-1 
         tri_list = copy_tri_explode(s_surf,tno,d_surf,divs,tri_list) 
      Next 
   Next 

   ; Create and return this explodable mesh's control variable.  
   ctrl.explode_ctrlT = New explode_ctrlT 
   ctrl\tri_list = tri_list 
   ctrl\mesh = d_mesh 
   ctrl\life = life 
   ctrl\fade_start = Ceil(Float(life) * FADE_START#) 
   ctrl\fader = 1.0 / (ctrl\fade_start + 1) 
   ctrl\exploding = False 
   ctrl\rotate = rot%

   Return ctrl 

End Function 


; 
; Adds an unwelded copy of a tri from the source surface to the destination 
; surface, performing any sub-division requested. 
; 
; Sub-division is done by calling this function recursively, splitting the 
; source tri into 4 unwelded tris each time, like so: 
; 
;       v1 
;       /\
;      /  \
;     /    \ 
;    /______\
;   / \    / \
;  /   \  /   \ 
; /_____\/_____\ 
;v0            v2 
; 
; Params: 
; s_surf - Source surface containing tri to be copied. 
; tri - Index of tri to be copied. 
; d_surf - Destination surface to copy the source tri to. 
; divs - Number of times to perform x4 sub-division on the source tri. 
; tri_list - Current top item in the linked list. 
; reset - True (default) to clear scratch pad mesh. 
; 
; Returns: 
; The current top item in the linked list. 
; 
Function copy_tri_explode.tri_linkT(s_surf,tri,d_surf,divs,tri_list.tri_linkT,reset%=True)
   Local t_mesh = CreateMesh() 
   Local t_surf = CreateSurface(t_mesh) 
;   HideEntity t_mesh 

   If reset Then ClearSurface(t_surf) 
   sv0 = TriangleVertex(s_surf,tri,0) 
   sv1 = TriangleVertex(s_surf,tri,1) 
   sv2 = TriangleVertex(s_surf,tri,2) 

   ; Get coords of all 3 vertices of source tri. 
   x0# = VertexX(s_surf,sv0) 
   y0# = VertexY(s_surf,sv0) 
   z0# = VertexZ(s_surf,sv0) 
   x1# = VertexX(s_surf,sv1)
   y1# = VertexY(s_surf,sv1) 
   z1# = VertexZ(s_surf,sv1) 
   x2# = VertexX(s_surf,sv2) 
   y2# = VertexY(s_surf,sv2) 
   z2# = VertexZ(s_surf,sv2) 

   ; Get normals of all 3 vertices of source tri. 
   nx0# = VertexNX(s_surf,sv0) 
   ny0# = VertexNY(s_surf,sv0) 
   nz0# = VertexNZ(s_surf,sv0) 
   nx1# = VertexNX(s_surf,sv1) 
   ny1# = VertexNY(s_surf,sv1) 
   nz1# = VertexNZ(s_surf,sv1) 
   nx2# = VertexNX(s_surf,sv2) 
   ny2# = VertexNY(s_surf,sv2) 
   nz2# = VertexNZ(s_surf,sv2) 

   ; Get tex coords of all 3 vertices of source tri. 
   u0# = VertexU(s_surf,sv0) 
   v0# = VertexV(s_surf,sv0) 
   w0# = VertexW(s_surf,sv0) 
   u1# = VertexU(s_surf,sv1) 
   v1# = VertexV(s_surf,sv1) 
   w1# = VertexW(s_surf,sv1) 
   u2# = VertexU(s_surf,sv2) 
   v2# = VertexV(s_surf,sv2) 
   w2# = VertexW(s_surf,sv2) 

   ; Get colour of all 3 vertices of source tri. 
   r0# = VertexRed(s_surf,sv0) 
   g0# = VertexGreen(s_surf,sv0) 
   b0# = VertexBlue(s_surf,sv0) 
   r1# = VertexRed(s_surf,sv1) 
   g1# = VertexGreen(s_surf,sv1) 
   b1# = VertexBlue(s_surf,sv1) 
   r2# = VertexRed(s_surf,sv2) 
   g2# = VertexGreen(s_surf,sv2) 
   b2# = VertexBlue(s_surf,sv2) 

   If divs 
      ; Calculate coords of the 3 discrete vertices used by sub-division. 
      x3# = x0 + ((x1-x0)/2) 
      y3# = y0 + ((y1-y0)/2) 
      z3# = z0 + ((z1-z0)/2) 
      x4# = x1 + ((x2-x1)/2) 
      y4# = y1 + ((y2-y1)/2) 
      z4# = z1 + ((z2-z1)/2) 
      x5# = x2 + ((x0-x2)/2) 
      y5# = y2 + ((y0-y2)/2) 
      z5# = z2 + ((z0-z2)/2) 

      ; Calculate normals of the 3 discrete vertices used by sub-division. 
      nx3# = nx0 + ((nx1-nx0)/2) 
      ny3# = ny0 + ((ny1-ny0)/2) 
      nz3# = nz0 + ((nz1-nz0)/2) 
      nx4# = nx1 + ((nx2-nx1)/2) 
      ny4# = ny1 + ((ny2-ny1)/2) 
      nz4# = nz1 + ((nz2-nz1)/2) 
      nx5# = nx2 + ((nx0-nx2)/2) 
      ny5# = ny2 + ((ny0-ny2)/2) 
      nz5# = nz2 + ((nz0-nz2)/2) 

      ; Calculate tex coords of the 3 discrete vertices used by sub-division. 
      u3# = u0 + ((u1-u0)/2) 
      v3# = v0 + ((v1-v0)/2) 
      w3# = w0 + ((w1-w0)/2) 
      u4# = u1 + ((u2-u1)/2) 
      v4# = v1 + ((v2-v1)/2) 
      w4# = w1 + ((w2-w1)/2) 
      u5# = u2 + ((u0-u2)/2) 
      v5# = v2 + ((v0-v2)/2) 
      w5# = w2 + ((w0-w2)/2) 

      ; Calculate colour of the 3 discrete vertices used by sub-division. 
      r3# = r0 + ((r1-r0)/2) 
      g3# = g0 + ((g1-g0)/2) 
      b3# = b0 + ((b1-b0)/2) 
      r4# = r1 + ((r2-r1)/2) 
      g4# = g1 + ((g2-g1)/2) 
      b4# = b1 + ((b2-b1)/2) 
      r5# = r2 + ((r0-r2)/2) 
      g5# = g2 + ((g0-g2)/2) 
      b5# = b2 + ((b0-b2)/2) 

      ; Add the 4 unwelded tris comprising the sub-division to the 
      ; temporary, scratch pad mesh surface. 
      tv0 = AddVertex(t_surf,x0,y0,z0) 
      tv3 = AddVertex(t_surf,x3,y3,z3) 
      tv5 = AddVertex(t_surf,x5,y5,z5) 
      tri0 = AddTriangle(t_surf,tv0,tv3,tv5) 
      VertexNormal t_surf,tv0,nx0,ny0,nz0 
      VertexNormal t_surf,tv3,nx3,ny3,nz3 
      VertexNormal t_surf,tv5,nx5,ny5,nz5 
      VertexColor t_surf,tv0,r0,g0,b0 
      VertexColor t_surf,tv3,r3,g3,b3 
      VertexColor t_surf,tv5,r5,g5,b5 
      VertexTexCoords t_surf,tv0,u0,v0,w0 
      VertexTexCoords t_surf,tv3,u3,v3,w3 
      VertexTexCoords t_surf,tv5,u5,v5,w5 

      tv1 = AddVertex(t_surf,x1,y1,z1) 
      tv4 = AddVertex(t_surf,x4,y4,z4) 
      tv3b = AddVertex(t_surf,x3,y3,z3) 
      tri1 = AddTriangle(t_surf,tv1,tv4,tv3b) 
      VertexNormal t_surf,tv1,nx1,ny1,nz1 
      VertexNormal t_surf,tv4,nx4,ny4,nz4 
      VertexNormal t_surf,tv3b,nx3,ny3,nz3 
      VertexColor t_surf,tv1,r1,g1,b1 
      VertexColor t_surf,tv4,r4,g4,b4 
      VertexColor t_surf,tv3b,r3,g3,b3 
      VertexTexCoords t_surf,tv1,u1,v1,w1 
      VertexTexCoords t_surf,tv4,u4,v4,w4 
      VertexTexCoords t_surf,tv3b,u3,v3,w3 

      tv2 = AddVertex(t_surf,x2,y2,z2) 
      tv5b = AddVertex(t_surf,x5,y5,z5) 
      tv4b = AddVertex(t_surf,x4,y4,z4) 
      tri2 = AddTriangle(t_surf,tv2,tv5b,tv4b) 
      VertexNormal t_surf,tv2,nx2,ny2,nz2 
      VertexNormal t_surf,tv5b,nx5,ny5,nz5 
      VertexNormal t_surf,tv4b,nx4,ny4,nz4 
      VertexColor t_surf,tv2,r2,g2,b2 
      VertexColor t_surf,tv5b,r5,g5,b5 
      VertexColor t_surf,tv4b,r4,g4,b4 
      VertexTexCoords t_surf,tv2,u2,v2,w2 
      VertexTexCoords t_surf,tv5b,u5,v5,w5 
      VertexTexCoords t_surf,tv4b,u4,v4,w4 

      tv3c = AddVertex(t_surf,x3,y3,z3) 
      tv4c = AddVertex(t_surf,x4,y4,z4) 
      tv5c = AddVertex(t_surf,x5,y5,z5) 
      tri3 = AddTriangle(t_surf,tv3c,tv4c,tv5c) 
      VertexNormal t_surf,tv3c,nx3,ny3,nz3 
      VertexNormal t_surf,tv4c,nx4,ny4,nz4 
      VertexNormal t_surf,tv5c,nx5,ny5,nz5 
      VertexColor t_surf,tv3c,r3,g3,b3 
      VertexColor t_surf,tv4c,r4,g4,b4 
      VertexColor t_surf,tv5c,r5,g5,b5 
      VertexTexCoords t_surf,tv3c,u3,v3,w3 
      VertexTexCoords t_surf,tv4c,u4,v4,w4 
      VertexTexCoords t_surf,tv5c,u5,v5,w5 

      divs = divs - 1 

      tri_list = copy_tri_explode(t_surf,tri0,d_surf,divs,tri_list,False) 
      tri_list = copy_tri_explode(t_surf,tri1,d_surf,divs,tri_list,False) 
      tri_list = copy_tri_explode(t_surf,tri2,d_surf,divs,tri_list,False) 
      tri_list = copy_tri_explode(t_surf,tri3,d_surf,divs,tri_list,False) 

   Else 
      dv0 = AddVertex(d_surf,x0,y0,z0) 
      dv1 = AddVertex(d_surf,x1,y1,z1) 
      dv2 = AddVertex(d_surf,x2,y2,z2) 

      ; Calculate and add a lone 'centre of tri' 
      ; vertex (needed for per-tri rotations). 
      tx# = x1 + ((x2-x1)/2) 
      ty# = y1 + ((y2-y1)/2) 
      tz# = z1 + ((z2-z1)/2) 
      cvx# = tx - ((tx-x0)/3) 
      cvy# = ty - ((ty-y0)/3) 
      cvz# = tz - ((tz-z0)/3) 
      AddVertex(d_surf,cvx,cvy,cvz) 

      real_tri = AddTriangle(d_surf,dv0,dv1,dv2) 

      VertexNormal d_surf,dv0,nx0,ny0,nz0 
      VertexNormal d_surf,dv1,nx1,ny1,nz1 
      VertexNormal d_surf,dv2,nx2,ny2,nz2 
      VertexColor d_surf,dv0,r0,g0,b0 
      VertexColor d_surf,dv1,r1,g1,b1 
      VertexColor d_surf,dv2,r2,g2,b2 
      VertexTexCoords d_surf,dv0,u0,v0,w0 
      VertexTexCoords d_surf,dv1,u1,v1,w1 
      VertexTexCoords d_surf,dv2,u2,v2,w2 

      ; Add this tri to the linked list. 
      link.tri_linkT = New tri_linkT 
      link\prev = tri_list 
      link\surf = d_surf 
      link\tri = real_tri

      ; this determine the initial velocity of the explosion
      r = Rnd(2.5,20) 
      link\dx = cvx/r 
      link\dy = cvy/r 
      link\dz = cvz/r 
      link\pitch = Rnd(-10,10) 
      link\yaw = Rnd(-10,10) 
      link\roll = Rnd(-10,10) 

      tri_list = link 
   EndIf 

   ClearSurface t_surf
   FreeEntity t_mesh 

   Return tri_list 

End Function 


; 
; Free all mem used by an explodable mesh created with copy_mesh_explode(). 
; 
; Params: 
; ctrl - Control variable of the explodable mesh to be freed. 
; 
Function free_mesh_explode(ctrl.explode_ctrlT) 
   this.tri_linkT = ctrl\tri_list 
   While this <> Null 
      delme.tri_linkT = this 
      this = delme\prev 
      Delete delme 
   Wend 
   FreeEntity ctrl\mesh 
   Delete ctrl 
End Function 


;####################################################################################################################




UNIT ODEImport;
(*************************************************************************
 *                                                                       *
 * Open Dynamics Engine, Copyright (C) 2001,2020 Russell L. Smith.       *
 *                All rights reserved. Web: www.ode.org                  *
 *                                                                       *
 * This library is free software; you can redistribute it and/or         *
 * modify it under the terms of EITHER:                                  *
 *   (1) The GNU Lesser General Public License as published by the Free  *
 *       Software Foundation; either version 2.1 of the License, or (at  *
 *       your option) any later version. The text of the GNU Lesser      *
 *       General Public License is included with this library in the     *
 *       file LICENSE.TXT.                                               *
 *   (2) The BSD-style license that is included with this library in     *
 *       the file LICENSE-BSD.TXT.                                       *
 *                                                                       *
 * This library is distributed in the hope that it will be useful,       *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the files    *
 * LICENSE.TXT and LICENSE-BSD.TXT for more details.                     *
 *                                                                       *
 *************************************************************************)


 (*************************************************************************
  Some notes;

  Sometimes it's easier and faster to refer to the members of the objects
  directly, like Body.Pos, Geom.Data or Body.Mass, instead of calling the built
  in routines. Be very careful if you do this, because some of the built in
  routines alter states when called.

  Examples

  bGeomSetBody(Geom, Body); // This method must be used so
  Geom.Body := Body; // DON'T DO THIS

  Setting the mass of a body is another example. Typically, reading members is
  fine, but before writing directly to member fields yourself, you should check
  the c source and see what the function/procedure does. The source can be found
  at http://www.q12.org/ode/

  *************************************************************************)

interface
// remove . from line below if you are using a generic ODE DLL
{$DEFINE VanillaODE}
{$IFNDEF VanillaODE}
   {$DEFINE PARODE}
{$ENDIF}

uses
  //System.Classes,
  //Import.ModuleLoader;
Classes, ModuleLoader;

const

  // ********************************************************************
  //
  //   ODE precision:
  //
  //   ODE can be run in Single or Double precision, Single is less precise,
  //   but requires less memory.
  //
  //   If you choose to run in Single mode, you must deploy the single precision
  //   dll (this is default)
  //
  //   If you choose to run in Double mode, you must deploy the double precision
  //   dll (named ode_double.dll and located in the dll directory)

  {$define cSINGLE}  //Insert . before "$" to make ODEImport double based
  {$IFDEF WIN32}
    {$IFDEF cSINGLE}
      ODEDLL = 'ode32s.dll';
    {$ELSE}
      ODEDLL = 'ode32d.dll';
    {$ENDIF}
  {$ENDIF}
  {$IFDEF WIN64}
    {$IFDEF cSINGLE}
      //ODEDLL = 'ode64s.dll';
      ODEDLL = 'ode64s0162.dll';
    {$ELSE}
      ODEDLL = 'ode64d.dll';
    {$ENDIF}
  {$ENDIF}

  {$IFDEF UNIX}
  ODEDLL = 'libode.so';
  {$ENDIF}
  {$IFDEF MACOS}
  ODEDLL = 'libode.dylib';
  {$ENDIF}
  {$IFDEF DARWIN} // MacOS X
  ODEDLL = 'libode.dylib';
  {$ENDIF}


const
// enum (* TRIMESH_FACE_NORMALS, TRIMESH_LAST_TRANSFORMATION ; *)
    TRIMESH_FACE_NORMALS = 0;
    TRIMESH_LAST_TRANSFORMATION = 1;

// Just generate any contacts (disables any contact refining).
    CONTACTS_UNIMPORTANT = $80000000;

// Change: New Type added, syntax enforcement
type TJointFlag = Integer;

// These consts now have defined types
const
  // if this flag is set, the joint was allocated in a joint group
    dJOINT_INGROUP: TJointFlag = 1;
  (* if this flag is set, the joint was attached with arguments (0,body).
     our convention is to treat all attaches as (body,0), i.e. so node[0].body
     is always nonzero, so this flag records the fact that the arguments were swapped *)
    dJOINT_REVERSE: TJointFlag = 2;
  (* if this flag is set, the joint can not have just one body attached to it,
     it must have either zero or two bodies attached *)
    dJOINT_TWOBODIES: TJointFlag = 4;


// Change: New Type added, syntax enforcement
type TdContactType = Integer;

// These consts now have defined types
const
    dContactMu2: TdContactType		= $0001;
    dContactFDir1: TdContactType	= $0002;
    dContactBounce: TdContactType   	= $0004;
    dContactSoftERP: TdContactType  	= $0008;
    dContactSoftCFM: TdContactType  	= $0010;
    dContactMotion1: TdContactType  	= $0020;
    dContactMotion2: TdContactType	= $0040;
    dContactMotionN: TdContactType	= $0080;
    dContactSlip1: TdContactType	= $0100;
    dContactSlip2: TdContactType	= $0200;

    dContactApprox0: TdContactType	= $0000;
    dContactApprox1_1: TdContactType	= $1000;
    dContactApprox1_2: TdContactType	= $2000;
    dContactApprox1: TdContactType	= $3000;

// Change: New Type added, syntax enforcement
type TBodyFlags = Integer;

// These consts now have defined types
const
  dxBodyFlagFiniteRotation: TBodyFlags = 1;		  // use finite rotations
  dxBodyFlagFiniteRotationAxis: TBodyFlags = 2;	// use finite rotations only along axis
  dxBodyDisabled: TBodyFlags = 4;			          // body is disabled
  dxBodyNoGravity: TBodyFlags = 8;              // body is not influenced by gravity
  dxBodyAutoDisable : TBodyFlags = 16; // enable auto-disable on body
  dxBodyLinearDamping : TBodyFlags = 32; // use linear damping
  dxBodyAngularDamping : TBodyFlags = 64; // use angular damping
  dxBodyMaxAngularSpeed : TBodyFlags = 128;// use maximum angular speed
  dxBodyGyroscopic : TBodyFlags = 256; // use gyroscopic term

type
  {$ifdef cSINGLE}
    TdReal = single;
  {$else}
    TdReal = double;
  {$endif}

  PdReal = ^TdReal;

  {define cODEDebugEnabled} // Debug mode

  (* Pointers to internal ODE structures to reproduce C++ classes in Delphi *)
  PdxJointGroup = ^TdxJointGroup;
  TdJointGroupID = PdxJointGroup;

  TdxJointGroup = record
     num : int64;
     //padding: array[1..4] of byte;
     //stack : pointer;
     stack : array [1..32] of byte;
  end;

  PdxJointLimitMotor = ^TdxJointLimitMotor;
  TdxJointLimitMotor = record
     vel,fmax : TdReal;  // powered joint: velocity, max force
     lostop,histop : TdReal; // joint limits, relative to initial position
     fudge_factor : TdReal; // when powering away from joint limits
     normal_cfm : TdReal; // cfm to use when not at a stop
     stop_erp,stop_cfm : TdReal; // erp and cfm for when at joint limit
     bounce : TdReal; // restitution factor
     // variables used between getInfo1() and getInfo2()
     limit : integer; // 0=free, 1=at lo limit, 2=at hi limit
     limit_err : TdReal; // if at limit, amount over limit
  end;

  TdRealArray = array[0..15] of TdReal;
  PdRealArray = ^TdRealArray;

  // typedef dReal dVector33[4];
  TdVector3 = array[0..3] of TdReal;//Q: Why isn't TdVector3 = array[0..2] of TdReal? A: Because of SIMD alignment.
  PdVector3 = ^TdVector3;

  Pd3Axis = ^Td3Axis;
  Td3Axis = array[0..2] of TdVector3;

  PdInteger3 = ^TdInteger3;
  TdInteger3 = array[0..2] of integer;

  PdxJointLimitMotor3 = ^TdxJointLimitMotor3;
  TdxJointLimitMotor3 = array[0..2] of TdxJointLimitMotor;
  
  PdxJointLimitMotor4 = ^TdxJointLimitMotor4;
  TdxJointLimitMotor4 = array[0..3] of TdxJointLimitMotor;

  // typedef dReal dVector4[4];
  TdVector4 = array[0..3] of TdReal;
  PdVector4 = ^TdVector4;

  // typedef dReal dMatrix3[4*3];
  TdMatrix3 = array[0..4*3-1] of TdReal;
  PdMatrix3 = ^TdMatrix3;

  TdMatrix3_As3x4 = array[0..2, 0..3] of TdReal;
  PdMatrix3_As3x4 = ^TdMatrix3_As3x4;

  // typedef dReal dMatrix4[4*4];
  TdMatrix4 = array[0..4*4-1] of TdReal;
  PdMatrix4 = ^TdMatrix4;

  // typedef dReal dMatrix6[8*6];
  TdMatrix6 = array[0..8*6-1] of TdReal;
  PdMatrix6 = ^TdMatrix6;

  // typedef dReal dQuaternion[4];
  TdQuaternion = TdVector4;//array[0..3] of TdReal;
  PdQuaternion = ^TdQuaternion;

  // No typedef for AABB
  TdAABB = array[0..5] of TdReal;


  TdMass = record
    mass : TdReal; // total mass of the rigid body
    c : TdVector4; // center of gravity position in body frame (x,y,z)
    I : TdMatrix3; // 3x3 inertia tensor in body frame, about POR
  end;
  PdMass = ^TdMass;

   TdxAutoDisable = record
      idle_time : TdReal; // time the body needs to be idle to auto-disable it
      idle_steps : integer; // steps the body needs to be idle to auto-disable it
      average_samples : longword; // size of the average_lvel and average_avel buffers
      linear_average_threashold : TdReal;  // linear (squared) average velocity threshold
      angular_average_threashold : TdReal;  // angular (squared) average velocity threshold

   end;
   TdxDampingParameters = record
      linear_scale : TdReal; // multiply the linear velocity by (1 - scale)
      angular_scale : TdReal; // multiply the angular velocity by (1 - scale)
      linear_threahold : TdReal; // linear (squared) average speed threshold
      angular_threashold : TdReal; // angular (squared) average speed threshold
   end;
   TdxContactParameters = record
      max_vel : TdReal;   // maximum correcting velocity
      min_depth : TdReal; // thickness of 'surface layer'
   end;
   TdxQuickStepParameters = record
      num_iterations : integer; // number of SOR iterations to perform
      w : TdReal; // the SOR over-relaxation parameter
   end;
  PdxGeom = ^TdxGeom;
  PPdxGeom = ^PdxGeom;

//  Whenever a body has its position or rotation changed during the
//  timestep, the callback will be called (with body as the argument).
//  Use it to know which body may need an update in an external
//  structure (like a 3D engine).
  TdMovedCallback = procedure(o1: PdxGeom); cdecl;

//  Per triangle callback. Allows the user to say if he wants a collision with
//  a particular triangle.
  TdTriCallback = function(TriMesh,RefObject : PdxGeom; TriangleIndex : integer) : integer;

//  Per object callback. Allows the user to get the list of triangles in 1
//  shot. Maybe we should remove this one.
  TdTriArrayCallback = procedure(TriMesh,RefObject : PdxGeom; TriIndices:PIntegerArray; TriCount : integer);

//  Allows the user to say if a ray collides with a triangle on barycentric
//  coords. The user can for example sample a texture with alpha transparency
//  to determine if a collision should occur.
  TdTriRayCallback = function(TriMesh,Ray : PdxGeom; TriangleIndex : integer; u,v:TdReal) : integer;


  PdxWorld = ^TdxWorld;

  PdObject = ^TdObject;
  PPdObject = ^PdObject;
  TdObject = record
    Padding   : array [1..8] of byte;
    World     : PdxWorld;  // world this object is in
    next      : PdObject;	// next object of this type in list
    tome      : PPdObject;	// pointer to previous object's next ptr
    tag       : integer;		// used by dynamics algorithms
    userdata  : pointer;		// user settable data
  end;


  PdxBody = ^TdxBody;
  PdxJoint= ^TdxJoint;
  TdJointID = PdxJoint;

  {$IFDEF PARODE}
  TdJointBreakCallback = procedure(joint: TdJointID); cdecl;

  TJointBreakMode = Integer;

  PdxJointBreakInfo = ^TdxJointBreakInfo;
  TdxJointBreakInfo = record
      flags : integer;
      b1MaxF:TdVector3; // maximum force on body 1
      b1MaxT:TdVector3; // maximum torque on body 1
      b2MaxF:TdVector3; // maximum force on body 2
      b2MaxT:TdVector3; // maximum torque on body 2
      callback:TdJointBreakCallback; // function that is called when this joint breaks
   end;
  {$ENDIF}

  PdxJointNode = ^TdxJointNode;
  TdxJointNode = record
     joint : PdxJoint; // pointer to enclosing dxJoint object
     body : PdxBody;   // *other* body this joint is connected to
     next : PdxJointNode; // next node in body's list of connected joints
  end;

  // info returned by getInfo1 function. the constraint dimension is m (<=6).
  // i.e. that is the total number of rows in the jacobian. `nub' is the
  // number of unbounded variables (which have lo,hi = -/+ infinity).
  TJointInfo1 = record
     m,nub : byte;
  end;

  {
  // info returned by getInfo2 function
  TJointInfo2 = record
    // integrator parameters: frames per second (1/stepsize), default error
    // reduction parameter (0..1).
     fps,erp : TdReal;
    // for the first and second body, pointers to two (linear and angular)
    // n*3 jacobian sub matrices, stored by rows. these matrices will have
    // been initialized to 0 on entry. if the second body is zero then the
    // J2xx pointers may be 0.
     J1l,J1a,J2l,J2a : pdReal;
    // elements to jump from one row to the next in J's
     rowskip : integer;
    // right hand sides of the equation J*v = c + cfm * lambda. cfm is the
    // "constraint force mixing" vector. c is set to zero on entry, cfm is
    // set to a constant value (typically very small or zero) value on entry.
     c,cfm : pdReal;
    // lo and hi limits for variables (set to -/+ infinity on entry).
     lo,hi : pdReal;
    // findex vector for variables. see the LCP solver interface for a
    // description of what this does. this is set to -1 on entry.
    // note that the returned indexes are relative to the first index of
    // the constraint.
     findex : pinteger;
  end;
  }
  TJointSureMaxInfo = record
     max_m : byte;
  end;

  TdxJoint = record
    baseObject : TdObject;   
    flags : integer;             // dJOINT_xxx flags
    node : array [0..1] of TdxJointNode;        // connections to bodies. node[1].body can be 0
    feedback : pointer;   // optional feedback structure
    lambda : array [0..5] of TdReal;            // lambda generated by last step
    //Padding: array [1..88] of byte;
    //JM: weird:
    //Info1 : TJointInfo1;
    //SureMaxInfo : TJointSureMaxInfo;
  end;
  TdxJointNull = TdxJoint;

  PdxJointBall = ^TdxJointBall;
  TdxJointBall = record
     BaseJoint : TdxJoint;
     anchor1 : TdVector3;// anchor w.r.t first body
     anchor2 : TdVector3;// anchor w.r.t second body
     erp : TdReal; // error reduction
     cfm : TdReal; // constraint force mix in
  end;

  PdxJointHinge = ^TdxJointHinge;
  TdxJointHinge = record
     BaseJoint : TdxJoint;
     anchor1 : TdVector3; // anchor w.r.t first body
     anchor2 : TdVector3; // anchor w.r.t second body
     axis1 : TdVector3; // axis w.r.t first body
     axis2 : TdVector3; // axis w.r.t second body
     qrel : tdQuaternion;  // initial relative rotation body1 -> body2
     limot : TdxJointLimitMotor; // limit and motor information
  end;

  PdxJointUniversial = ^TdxJointUniversial;
  TdxJointUniversial = record
     BaseJoint : TdxJoint;
     anchor1 : TdVector3; // anchor w.r.t first body
     anchor2 : TdVector3; // anchor w.r.t second body
     axis1 : TdVector3; // axis w.r.t first body
     axis2 : TdVector3; // axis w.r.t second body
     qrel1 : tdQuaternion; // initial relative rotation body1 -> virtual cross piece
     qrel2 : tdQuaternion; // initial relative rotation virtual cross piece -> body2
     limot1 : TdxJointLimitMotor; // limit and motor information for axis1
     limot2 : TdxJointLimitMotor;// limit and motor information for axis2
  end;

  PdxJointPR = ^TdxJointPR;
  TdxJointPR = record
     BaseJoint : TdxJoint;

     anchor2:TdVector3;     ///< @brief Position of the rotoide articulation
                            ///<        w.r.t second body.
                            ///< @note Position of body 2 in world frame +
                            ///< anchor2 in world frame give the position
                            ///< of the rotoide articulation
     axisR1:TdVector3 ;     ///< axis of the rotoide articulation w.r.t first body.
                            ///< @note This is considered as axis1 from the parameter
                            ///< view.
     axisR2:TdVector3 ;     ///< axis of the rotoide articulation w.r.t second body.
                            ///< @note This is considered also as axis1 from the
                            ///< parameter view
     axisP1:TdVector3;      ///< axis for the prismatic articulation w.r.t first body.
                            ///< @note This is considered as axis2 in from the parameter
                            ///< view
     qrel:TdQuaternion;     ///< initial relative rotation body1 -> body2.
     offset:TdVector3;      ///< @brief vector between the body1 and the rotoide
                            ///< articulation.
                            ///<
                            ///< Going from the first to the second in the frame
                            ///<  of body1.
                            ///< That should be aligned with body1 center along axisP
                            ///< This is calculated when the axis are set.
     limotR:TdxJointLimitMotor; ///< limit and motor information for the rotoide articulation.
     limotP:TdxJointLimitMotor; ///< limit and motor information for the prismatic articulation.
  end;

  PdxJointPiston = ^TdxJointPiston;
  TdxJointPiston = record
     BaseJoint : TdxJoint;

     axis1:TdVector3;          ///< Axis of the prismatic and rotoide w.r.t first body
     axis2:TdVector3;          ///< Axis of the prismatic and rotoide w.r.t second body

     qrel:TdQuaternion;        ///< Initial relative rotation body1 -> body2

  /// Anchor w.r.t first body.
  /// This is the same as the offset for the Slider joint
  /// @note To find the position of the anchor when the body 1 has moved
  ///       you must add the position of the prismatic joint
  ///       i.e anchor = R1 * anchor1 + dJointGetPistonPosition() * (R1 * axis1)
     anchor1:TdVector3;
     anchor2:TdVector3;        //< anchor w.r.t second body

  /// limit and motor information for the prismatic
  /// part of the joint
     limotP:TdxJointLimitMotor;

  /// limit and motor information for the rotoide
  /// part of the joint
     limotR:TdxJointLimitMotor;
  end;

  PdxJointSlider = ^TdxJointSlider;
  TdxJointSlider = record
     BaseJoint : TdxJoint;
     axis1:TdVector3;		// axis w.r.t first body
     qrel:TdQuaternion;		// initial relative rotation body1 -> body2
     offset:TdVector3;		// point relative to body2 that should be
                				// aligned with body1 center along axis1
     limot:TdxJointLimitMotor;	// limit and motor information
  end;

  PdxJointHinge2 = ^TdxJointHinge2;
  TdxJointHinge2 = record
     BaseJoint : TdxJoint;

     anchor1:TdVector3 ;		// anchor w.r.t first body
     anchor2:TdVector3 ;		// anchor w.r.t second body
     axis1:TdVector3;		// axis 1 w.r.t first body
     axis2:TdVector3;		// axis 2 w.r.t second body
     c0,s0:TdReal;			// cos,sin of desired angle between axis 1,2
     v1,v2:TdVector3;		// angle ref vectors embedded in first body
     w1,w2:TdVector3;
     limot1:TdxJointLimitMotor;	// limit+motor info for axis 1
     limot2:TdxJointLimitMotor;	// limit+motor info for axis 2
     susp_erp,susp_cfm:TdReal;	// suspension parameters (erp,cfm)
  end;

  TdxJointAMotor = record
     BaseJoint : TdxJoint;

     mode:integer;			// a dAMotorXXX constant
     num:integer;			// number of axes (0..3)
     rel:TdInteger3;			// what the axes are relative to (global,b1,b2)
     axis:Td3Axis;		// three axes

     references: array[0..1] of TdVector3;
     angle: array[0..2] of TdReal;
     limot:TdxJointLimitMotor4;	// limit+motor info for axes
  end;

  TdxJointLMotor = record
     BaseJoint : TdxJoint;

     num: integer;
     rel:TdInteger3;
     axis:Td3Axis;		// three axes
     limot:TdxJointLimitMotor3;	// limit+motor info for axes
  end;

  TdxJointPlane2D = record
     BaseJoint : TdxJoint;
     row_motor_x:integer;
     row_motor_y:integer;
     row_motor_angle:integer;
     motor_x:TdxJointLimitMotor;
     motor_y:TdxJointLimitMotor;
     motor_angle:TdxJointLimitMotor;
  end;

  TdxJointFixed = record
     BaseJoint : TdxJoint;
     qrel:TdQuaternion;		// initial relative rotation body1 -> body2
     offset:TdVector3;		// relative offset between the bodies
     erp:TdReal;			// error reduction parameter
     cfm:TdReal;			// constraint force mix-in
  end;

  // position vector and rotation matrix for geometry objects that are not
  // connected to bodies.
  PdxPosR = ^TdxPosR;
  TdxPosR = record
    pos : TdVector3;
    R : TdMatrix3;
  end;



























  TdxBody = record
    BaseObject : TdObject;
    firstjoint : TdJointID;	// list of attached joints
    flags : integer;			  // some dxBodyFlagXXX flags
    geom : PdxGeom;          // first collision geom associated with body
    mass : TdMass;			    // mass parameters about POR
    invI : TdMatrix3 ;		  // inverse of mass.I
    invMass : TdReal;		    // 1 / mass.mass
    posr : TdxPosR;			  // position and orientation of point of reference
    q : TdQuaternion;		    // orientation quaternion
    lvel,avel : TdVector3;	// linear and angular velocity of POR
    facc,tacc : TdVector3 ;	// force and torque accululators
    finite_rot_axis : TdVector3 ;	// finite rotation axis, unit length or 0=none
    adis : TdxAutoDisable; // auto-disable parameters
    adis_timeleft : TdReal; // time left to be idle
    adis_stepsleft : integer;  // steps left to be idle
    average_lvel_buffer : pdVector3; // buffer for the linear average velocity calculation
    average_avel_buffer : pdVector3; // buffer for the angular average velocity calculation
    average_counter : longword; // counter/index to fill the average-buffers
    average_ready : integer; // indicates ( with = 1 ), if the Body's buffers are ready for average-calculations
    moved_callback : TdMovedCallback; // let the user know the body moved
    dampingp : TdxDampingParameters; // damping parameters, depends on flags
    max_angular_speed : TdReal; // limit the angular velocity to this magnitude
  end;

  TBodyList = class(TList)
  private
    function GetItems(i: integer): PdxBody;
    procedure SetItems(i: integer; const Value: PdxBody);
  public
    property Items[i : integer] : PdxBody read GetItems write SetItems; default;
    procedure DeleteAllBodies;
  end;


(*struct dxWorld : public dBase {
  dxBody *firstbody;		// body linked list
  dxJoint *firstjoint;	// joint linked list
  int nb,nj;			      // number of bodies and joints in lists
  dVector3 gravity;		  // gravity vector (m/s/s)
  dReal global_erp;		  // global error reduction parameter
  dReal global_cfm;		  // global costraint force mixing parameter
};*)

  TdxWorld = record //(TdBase)
    padding_threads : array [0..39] of byte;
    //  devia ser este mas não funciona
    //padding_threads : array [0..31] of byte;
    firstbody : PdxBody;		// body linked list
    firstjoint : PdxJoint;	// joint linked list
    nb,nj : integer;			  // number of bodies and joints in lists
    gravity : TdVector3;		// gravity vector (m/s/s)
    global_erp : TdReal;		// global error reduction parameter
    global_cfm : TdReal;		// global costraint force mixing parameter
    adis : TdxAutoDisable;
    body_flags : integer;
    islands_max_threads : integer;
    wmem : pointer;
    qs : TdxQuickStepParameters;
    contactp : TdxContactParameters;
    dampingp : TdxDampingParameters;
    max_angular_speed : TdReal;
    userdata : pointer;
  end;


  TdJointFeedback = record
    f1 : TdVector3;       // force that joint applies to body 1
    t1 : TdVector3;       // torque that joint applies to body 1
    f2 : TdVector3;       // force that joint applies to body 2
    t2 : TdVector3;       // torque that joint applies to body 2
  end;

  PTdJointFeedback = ^TdJointFeedback;

  TdErrorType =
    (d_ERR_UNKNOWN, // unknown error
     d_ERR_IASSERT, // user assertion failed */
     d_ERR_UASSERT, // user assertion failed */
     d_ERR_LCP);


  TdJointTypeNumbers =
    (dJointTypeNone,		// or "unknown"
    dJointTypeBall,
    dJointTypeHinge,
    dJointTypeSlider,
    dJointTypeContact,
    dJointTypeUniversal,
    dJointTypeHinge2,
    dJointTypeFixed,
    dJointTypeNull,
    dJointTypeAMotor,
    dJointTypeLMotor,
    dJointTypePlane2D,
    dJointTypePR,
    dJointTypePU,
    dJointTypePiston,
    dJointTypeDBall,
    dJointTypeDHinge,
    dJointTypeTransmission
    );

  TdAngularMotorModeNumbers =
    (dAMotorUser,
     dAMotorEuler);

  TdTransmissioModeNumbers =
    (dTransmissionParallelAxes,
     dTransmissionIntersectingAxes,
     dTransmissionChainDrive);

  TdSurfaceParameters = record
    // must always be defined
    mode : integer;
    mu : TdReal;

    // only defined if the corresponding flag is set in mode
    mu2,
    rho,
    rho2,
    rhoN,

    bounce,
    bounce_vel,
    soft_erp,
    soft_cfm,
    motion1,motion2,motionN,
    slip1,slip2 : TdReal
  end;

  TdContactGeom = record
    pos : TdVector3;
    normal : TdVector3;
    depth : TdReal;
    g1,g2 : PdxGeom;
    side1,side2 : integer;
  end;

  PdContactGeom = ^TdContactGeom;

  TdContact = record
    surface : TdSurfaceParameters;
    geom : TdContactGeom;
    fdir1 : TdVector3;
  end;
  PdContact = ^TdContact;

  // Collission callback structure
  TdNearCallback = procedure(data : pointer; o1, o2 : PdxGeom); cdecl;


  TdColliderFn = function(o1, o2 : PdxGeom; flags : Integer;
                  contact : PdContactGeom; skip : Integer) : Integer; cdecl;
  TdGetColliderFnFn = function(num : Integer) : TdColliderFn; cdecl;
  TdGetAABBFn = procedure(g : PdxGeom; var aabb : TdAABB); cdecl;
  TdGeomDtorFn = procedure(o : PdxGeom); cdecl;
  TdAABBTestFn = function(o1, o2 : PdxGeom; const aabb2 : TdAABB) : Integer; cdecl;

(*typedef struct dGeomClass {
  int bytes;
  dGetColliderFnFn *collider;
  dGetAABBFn *aabb;
  dAABBTestFn *aabb_test;
  dGeomDtorFn *dtor;
} dGeomClass;*)

  TdGeomClass = record
    bytes : integer;                 // extra storage size
    collider : TdGetColliderFnFn;    // collider function
    aabb : TdGetAABBFn;       // bounding box function
    aabb_test : TdAABBTestFn; // aabb tester, can be 0 for none
    dtor : TdGeomDtorFn;      // destructor, can be 0 for none
  end;

  PdGeomClass = ^TdGeomClass;

  (*struct dxSpace : public dBase {
  int type;			// don't want to use RTTI
  virtual void destroy()=0;
  virtual void add (dGeomID)=0;
  virtual void remove (dGeomID)=0;
  virtual void collide (void *data, dNearCallback *callback)=0;
  virtual int query (dGeomID)=0;
};*)

  PdxSpace = ^TdxSpace;

  TdRealHugeArray = array[0..65535] of TdReal;
  PdRealHugeArray = ^TdRealHugeArray;

  // Tri-list collider
  TdIntegerArray = array[0..65535] of Integer;
  PdIntegerArray = ^TdIntegerArray;

  TdVector3Array = array[0..65535] of TdVector3;
  PdVector3Array = ^TdVector3Array;

(*struct dxTriMeshData{
	Model BVTree;
	MeshInterface Mesh;

    dxTriMeshData();
    ~dxTriMeshData();

    void Build(const void* Vertices, int VertexStide, int VertexCount,
	       const void* Indices, int IndexCount, int TriStride,
	       const void* Normals,
	       bool Single);

        /* aabb in model space */
        dVector3 AABBCenter;
        dVector3 AABBExtents;

    /* data for use in collison resolution */
    const void* Normals;
    Matrix4x4   last_trans;
};*)
  TdxTriMeshData = record
    unknown : array [1..200] of byte; //
  end;
  PdxTriMeshData = ^TdxTriMeshData;

  TdxHeightfieldData = record
    m_fWidth : TdReal;
    m_fDepth : TdReal;
    m_fSampleWidth : TdReal;
    m_fSampleDepth : TdReal;
    m_fSampleZXAspect: TdReal;
    m_fInvSampleWidth : TdReal;
    m_fInvSampleDepth : TdReal;
    m_fHalfWidth : TdReal;
    m_fHalfDepth : TdReal;
    m_fMinHeight : TdReal;
    m_fMaxHeight : TdReal;
    m_fThickness : TdReal;
    m_fScale : TdReal;
    m_fOffset : TdReal;
    m_nWidthSamples : integer;
    m_nDepthSamples : integer;
    m_bCopyHeightData : integer;
    m_bWrapMode : integer;
    m_nGetHeightMode : integer;
    m_pHeightData : pointer;
    m_pUserData : pointer;
    m_contacts : array[0..9] of TdContactGeom;
    m_pGetHeightCallback : TdReal;
  end;
  PdxHeightfieldData = ^TdxHeightfieldData;


(*//simple space - reports all n^2 object intersections
struct dxSimpleSpace : public dxSpace {
  dGeomID first;
  void destroy();
  void add (dGeomID);
  void remove (dGeomID);
  void collide (void *data, dNearCallback *callback);
  int query (dGeomID);
};*)


  PdxSimpleSpace = ^TdxSimpleSpace;

(*//currently the space 'container' is just a list of the geoms in the space.
struct dxHashSpace : public dxSpace {
  dxGeom *first;
  int global_minlevel;	// smallest hash table level to put AABBs in
  int global_maxlevel;	// objects that need a level larger than this will be
			// put in a "big objects" list instead of a hash table
  void destroy();
  void add (dGeomID);
  void remove (dGeomID);
  void collide (void *data, dNearCallback *callback);
  int query (dGeomID);
};*)

  PdxHashSpace = ^TdxHashSpace;

  (*typedef struct dGeomSpaceData {
  dGeomID next;
} dGeomSpaceData; *)

  //TODO: delete this
  {TdGeomSpaceData = record
     next : PdxGeom;
  end; }

  (*// common data for all geometry objects. the class-specific data area follows
  // this structure. pos and R will either point to a separately allocated
  // buffer (if body is 0 - pos points to the dxPosR object) or to the pos and
  // R of the body (if body nonzero).
  struct dxGeom {		// a dGeomID is a pointer to this
    dxGeomClass *_class;	// class of this object
    void *data;		// user data pointer
    dBodyID body;		// dynamics body associated with this object (if any)
    dReal *pos;		// pointer to object's position vector
    dReal *R;		// pointer to object's rotation matrix
    dSpaceID spaceid;	// the space this object is in
    dGeomSpaceData space;	// reserved for use by space this object is in
    dReal *space_aabb;	// ptr to aabb array held by dSpaceCollide() fn
    // class-specific data follows here, with proper alignment.
  };*)

  TdxGeom = record // a dGeomID is a pointer to this
     padding : array [1..8] of byte;
     _type : integer;	// class of this object


     gflags : integer;

     data : pointer;		// user data pointer
     Body : PdxBody ;		// dynamics body associated with this object (if any)
     body_next : PdxGeom; // next geom in body's linked list of associated geoms
     final_posr : PdxPosR; // final position of the geom in world coordinates
     offset_posr : PdxPosR; // offset from body in local coordinates

     next : PdxGeom;
     tome : PPdxGeom;
     next_ex : PdxGeom;
     tome_ex : PPdxGeom;
     parent_space : Pdxspace;
     aabb : TdAABB;
     category_bits,collide_bits : longword;
  end;

  TGeomList = class(TList)
  private
    function GetItems(i: integer): PdxGeom;
    procedure SetItems(i: integer; const Value: PdxGeom);
  public
    property Items[i : integer] : PdxGeom read GetItems write SetItems; default;
    procedure DeleteAllGeoms(DeleteDataAsObject : boolean=false);
  end;


  TdxSpace = record
    baseGeom : TdxGeom;
    count : integer;
    first : PdxGeom;
    cleanup : integer;
    sublevel: integer;
    tls_kind: integer;
    current_index : integer;
    current_geom : PdxGeom;
    lock_count : integer;
  end;

  // Copies settings from TdxSpace, beacause pascal doeasn't do inheritance
  // for records
  TdxSimpleSpace = TdxSpace;

  // Copies settings from TdxSpace, beacause pascal doeasn't do inheritance
  // for records
  TdxHashSpace = record
    BaseSpace : TdxSpace;
    //padding: byte;
    global_minlevel : integer;
    global_maxlevel : integer;
  end;

  TdxQuadTreeSpace = record
    BaseSpace : TdxSpace;
    Blocks : pointer;
    DirtyList : pointer;
    padding: array [1..48] of byte;
  end;

//  TJointParams = (
// parameters for limits and motors
// Change: New Type added, sintax enforcement
    TJointParams = Integer;

// These consts now have defined types

{$IFDEF PARODE}
  const
     dJOINT_BREAK_UNKNOWN:TJointBreakMode =      $0000;
     dJOINT_BROKEN:TJointBreakMode =             $0001;
     dJOINT_DELETE_ON_BREAK:TJointBreakMode =    $0002;
     dJOINT_BREAK_AT_B1_FORCE:TJointBreakMode =  $0004;
     dJOINT_BREAK_AT_B1_TORQUE:TJointBreakMode = $0008;
     dJOINT_BREAK_AT_B2_FORCE:TJointBreakMode =  $0010;
     dJOINT_BREAK_AT_B2_TORQUE:TJointBreakMode = $0020;
{$ENDIF}

  const
    _priv_dParamLoStop  = $000;
    _priv_dParamLoStop2 = $100;
    _priv_dParamLoStop3 = $200;
  const
    dParamLoStop: TJointParams = _priv_dParamLoStop;
    dParamHiStop: TJointParams = _priv_dParamLoStop + 1;
    dParamVel: TJointParams = _priv_dParamLoStop + 2;
    dParamLoVel: TJointParams = _priv_dParamLoStop + 3;
    dParamHiVel: TJointParams = _priv_dParamLoStop + 4;
    dParamFMax: TJointParams = _priv_dParamLoStop + 5;
    dParamFudgeFactor: TJointParams = _priv_dParamLoStop + 6;
    dParamBounce: TJointParams = _priv_dParamLoStop + 7;
    dParamCFM: TJointParams = _priv_dParamLoStop + 8;
    dParamStopERP: TJointParams = _priv_dParamLoStop + 9;
    dParamStopCFM: TJointParams = _priv_dParamLoStop + 10;
    // parameters for suspension
    dParamSuspensionERP: TJointParams = _priv_dParamLoStop + 11;
    dParamSuspensionCFM: TJointParams = _priv_dParamLoStop + 12;
    dParamERP: TJointParams = _priv_dParamLoStop + 13;

    dParamGroup1: TJointParams = $000;
    dParamLoStop1: TJointParams = _priv_dParamLoStop;
    dParamHiStop1: TJointParams = _priv_dParamLoStop + 1;
    dParamVel1: TJointParams = _priv_dParamLoStop + 2;
    dParamLoVel1: TJointParams = _priv_dParamLoStop + 3;
    dParamHiVel1: TJointParams = _priv_dParamLoStop + 4;
    dParamFMax1: TJointParams = _priv_dParamLoStop + 5;
    dParamFudgeFactor1: TJointParams = _priv_dParamLoStop + 6;
    dParamBounce1: TJointParams = _priv_dParamLoStop + 7;
    dParamCFM1: TJointParams = _priv_dParamLoStop + 8;
    dParamStopERP1: TJointParams = _priv_dParamLoStop + 9;
    dParamStopCFM1: TJointParams = _priv_dParamLoStop + 10;
    // parameters for suspension
    dParamSuspensionERP1: TJointParams = _priv_dParamLoStop + 11;
    dParamSuspensionCFM1: TJointParams = _priv_dParamLoStop + 12;
    dParamERP1: TJointParams = _priv_dParamLoStop + 13;

    // SECOND AXEL
    // parameters for limits and motors
    dParamGroup2: TJointParams = $100;
    dParamLoStop2: TJointParams = _priv_dParamLoStop2;
    dParamHiStop2: TJointParams = _priv_dParamLoStop2 + 1;
    dParamVel2: TJointParams = _priv_dParamLoStop2 + 2;
    dParamFMax2: TJointParams = _priv_dParamLoStop2 + 3;
    dParamFudgeFactor2: TJointParams = _priv_dParamLoStop2 + 4;
    dParamBounce2: TJointParams = _priv_dParamLoStop2 + 5;
    dParamCFM2: TJointParams = _priv_dParamLoStop2 + 6;
    dParamStopERP2: TJointParams = _priv_dParamLoStop2 + 7;
    dParamStopCFM2: TJointParams = _priv_dParamLoStop2 + 8;
    // parameters for suspension
    dParamSuspensionERP2: TJointParams = _priv_dParamLoStop2 + 9;
    dParamSuspensionCFM2: TJointParams = _priv_dParamLoStop2 + 10;
    dParamERP2: TJointParams = _priv_dParamLoStop2 + 11;

    // THIRD AXEL
    // parameters for limits and motors
    dParamGroup3: TJointParams = $200;
    dParamLoStop3: TJointParams = _priv_dParamLoStop3;
    dParamHiStop3: TJointParams = _priv_dParamLoStop3 + 1;
    dParamVel3: TJointParams = _priv_dParamLoStop3 + 2;
    dParamFMax3: TJointParams = _priv_dParamLoStop3 + 3;
    dParamFudgeFactor3: TJointParams = _priv_dParamLoStop3 + 4;
    dParamBounce3: TJointParams = _priv_dParamLoStop3 + 5;
    dParamCFM3: TJointParams = _priv_dParamLoStop3 + 6;
    dParamStopERP3: TJointParams = _priv_dParamLoStop3 + 7;
    dParamStopCFM3: TJointParams = _priv_dParamLoStop3 + 8;
    // parameters for suspension
    dParamSuspensionERP3: TJointParams = _priv_dParamLoStop3 + 9;
    dParamSuspensionCFM3: TJointParams = _priv_dParamLoStop3 + 10;
    dParamERP3: TJointParams = _priv_dParamLoStop3 + 11;
    dParamGroup: TJointParams = $100;

  // added by PAR
   {$IFDEF PARODE}
   function dSphereGetClass: integer; cdecl; external ODEDLL;
   function dBoxGetClass: integer; cdecl; external ODEDLL;
   function dCylinderGetClass: integer; cdecl; external ODEDLL;
   function dCapsuleGetClass: integer; cdecl; external ODEDLL;
   function dRayGetClass: integer; cdecl; external ODEDLL;
   function dPlaneGetClass: integer; cdecl; external ODEDLL;
   function dConvexGetClass: integer; cdecl; external ODEDLL;
   function dTriMeshGetClass: integer; cdecl; external ODEDLL;
   function dHeightfieldGetClass: integer; cdecl; external ODEDLL;
   function dGeomTransformGetClass: integer; cdecl; external ODEDLL;
   {$ENDIF}

   procedure dInitODE; cdecl; external ODEDLL;
   function dInitODE2(uiInitFlags: longword): integer; cdecl; external ODEDLL;

  procedure dCloseODE; cdecl; external ODEDLL;


  //----- dWorld -----
  function dWorldCreate: PdxWorld; cdecl; external ODEDLL;
  procedure dWorldDestroy(const World: PdxWorld); cdecl; external ODEDLL;
  function dWorldGetCFM(const World: PdxWorld): TdReal; cdecl; external ODEDLL;
  function dWorldGetERP(const World: PdxWorld): TdReal; cdecl; external ODEDLL;
  procedure dWorldGetGravity(const World: PdxWorld; var gravity: TdVector3); cdecl; external ODEDLL;
  procedure dWorldImpulseToForce(const World: PdxWorld; const stepsize, ix, iy, iz: TdReal; var force: TdVector3); cdecl; external ODEDLL;
  procedure dWorldSetCFM(const World: PdxWorld; cfm: TdReal); cdecl; external ODEDLL;
  procedure dWorldSetERP(const World: PdxWorld; erp: TdReal); cdecl; external ODEDLL;
  procedure dWorldSetGravity(const World: PdxWorld; const x, y, z: TdReal); cdecl; external ODEDLL;
  procedure dWorldSetContactMaxCorrectingVel(const World: PdxWorld; const vel: TdReal); cdecl; external ODEDLL;
  function dWorldGetContactMaxCorrectingVel(const World: PdxWorld): TdReal; cdecl; external ODEDLL;
  procedure dWorldSetContactSurfaceLayer(const World: PdxWorld; const depth: TdReal); cdecl; external ODEDLL;
  function dWorldGetContactSurfaceLayer(const World: PdxWorld): TdReal; cdecl; external ODEDLL;
  procedure dWorldExportDIF(const World: PdxWorld; fileHandle: cardinal; const world_name:PAnsiChar); cdecl; external ODEDLL;

  // Damping
  function dWorldGetLinearDampingThreshold(const World: PdxWorld): TdReal; cdecl; external ODEDLL;
  procedure dWorldSetLinearDampingThreshold(const World: PdxWorld; const threshold: TdReal); cdecl; external ODEDLL;
  function dWorldGetAngularDampingThreshold(const World: PdxWorld): TdReal; cdecl; external ODEDLL;
  procedure dWorldSetAngularDampingThreshold(const World: PdxWorld; const threshold: TdReal); cdecl; external ODEDLL;
  function dWorldGetLinearDamping(const World: PdxWorld): TdReal; cdecl; external ODEDLL;
  procedure dWorldSetLinearDamping(const World: PdxWorld; const scale: TdReal); cdecl; external ODEDLL;
  function dWorldGetAngularDamping(const World: PdxWorld): TdReal; cdecl; external ODEDLL;
  procedure dWorldSetAngularDamping(const World: PdxWorld; const scale: TdReal); cdecl; external ODEDLL;
  procedure dWorldSetDamping(const World: PdxWorld; const linear_scale,angular_scale: TdReal); cdecl; external ODEDLL;
  function dWorldGetMaxAngularSpeed(const World: PdxWorld): TdReal; cdecl; external ODEDLL;
  procedure dWorldSetMaxAngularSpeed(const World: PdxWorld; const max_speed: TdReal); cdecl; external ODEDLL;
  // Step
  //procedure dWorldStep(const World: PdxWorld; const stepsize: TdReal); cdecl; external ODEDLL;
  function dWorldStep(const World: PdxWorld; const stepsize: TdReal): integer; cdecl; external ODEDLL;
  // QuickStep
  function dWorldQuickStep(const World: PdxWorld; const stepsize: TdReal): integer; cdecl; external ODEDLL;
  procedure dWorldSetQuickStepNumIterations(const World: PdxWorld; const num: integer); cdecl; external ODEDLL;
  function dWorldGetQuickStepNumIterations(const World: PdxWorld): integer; cdecl; external ODEDLL;
  procedure dWorldSetQuickStepW(const World: PdxWorld; const param: TdReal); cdecl; external ODEDLL;
  function dWorldGetQuickStepW(const World: PdxWorld): TdReal; cdecl; external ODEDLL;
  // Auto-disable functions
  //function dWorldGetAutoDisableLinearAverageThreshold(const World: PdxWorld): TdReal; cdecl; external ODEDLL;
  //procedure dWorldSetAutoDisableLinearAverageThreshold(const World: PdxWorld; linear_average_threshold: TdReal); cdecl; external ODEDLL;
  //function dWorldGetAutoDisableAngularAverageThreshold(const World: PdxWorld): TdReal; cdecl; external ODEDLL;
  //procedure dWorldSetAutoDisableAngularAverageThreshold(const World: PdxWorld; linear_average_threshold: TdReal); cdecl; external ODEDLL;
  function dWorldGetAutoDisableAverageSamplesCount(const World: PdxWorld): TdReal; cdecl; external ODEDLL;
  procedure dWorldSetAutoDisableAverageSamplesCount(const World: PdxWorld; linear_average_threshold: TdReal); cdecl; external ODEDLL;

  function dWorldGetAutoDisableLinearThreshold(const World: PdxWorld): TdReal; cdecl; external ODEDLL;
  procedure dWorldSetAutoDisableLinearThreshold(const World: PdxWorld; linThreshold: TdReal); cdecl; external ODEDLL;
  function dWorldGetAutoDisableAngularThreshold(const World: PdxWorld): TdReal; cdecl; external ODEDLL;
  procedure dWorldSetAutoDisableAngularThreshold(const World: PdxWorld; angThreshold: TdReal); cdecl; external ODEDLL;
  function dWorldGetAutoDisableSteps(const World: PdxWorld): Integer; cdecl; external ODEDLL;
  procedure dWorldSetAutoDisableSteps(const World: PdxWorld; steps: Integer); cdecl; external ODEDLL;
  function dWorldGetAutoDisableTime(const World: PdxWorld): TdReal; cdecl; external ODEDLL;
  procedure dWorldSetAutoDisableTime(const World: PdxWorld; time: TdReal); cdecl; external ODEDLL;
  function dWorldGetAutoDisableFlag(const World: PdxWorld): Integer; cdecl; external ODEDLL;
  procedure dWorldSetAutoDisableFlag(const World: PdxWorld; do_auto_disable: Integer); cdecl; external ODEDLL;


  //----- dBody -----
  procedure dBodyAddForce(const Body: PdxBody; const fx, fy, fz: TdReal); cdecl; external ODEDLL;
  procedure dBodyAddForceAtPos(const Body: PdxBody; const fx, fy, fz, px, py, pz: TdReal); cdecl; external ODEDLL;
  procedure dBodyAddForceAtRelPos(const Body: PdxBody; const fx, fy, fz, px, py, pz: TdReal); cdecl; external ODEDLL;
  procedure dBodyAddRelForce(const Body: PdxBody; const fx, fy, fz: TdReal); cdecl; external ODEDLL;
  procedure dBodyAddRelForceAtPos(const Body: PdxBody; const fx, fy, fz, px, py, pz: TdReal); cdecl; external ODEDLL;
  procedure dBodyAddRelForceAtRelPos(const Body: PdxBody; const fx, fy, fz, px, py, pz: TdReal); cdecl; external ODEDLL;
  procedure dBodyAddRelTorque(const Body: PdxBody; const fx, fy, fz: TdReal); cdecl; external ODEDLL;
  procedure dBodyAddTorque(const Body: PdxBody; const fx, fy, fz: TdReal); cdecl; external ODEDLL;

  function dBodyCreate(const World: PdxWorld): PdxBody; cdecl; external ODEDLL;
  procedure dBodyDestroy(const Body: PdxBody); cdecl; external ODEDLL;
  procedure dBodyDisable(const Body: PdxBody); cdecl; external ODEDLL;
  procedure dBodyEnable(const Body: PdxBody); cdecl; external ODEDLL;
  function dBodyGetAngularVel(const Body: PdxBody): PdVector3; cdecl; external ODEDLL;
  procedure dBodyGetFiniteRotationAxis(const Body: PdxBody; var result: TdVector3); cdecl; external ODEDLL;
  function dBodyGetFiniteRotationMode(const Body: PdxBody): Integer; cdecl; external ODEDLL;
  function dBodyGetForce(const Body: PdxBody): PdVector3; cdecl; external ODEDLL;
  function dBodyGetGravityMode(const Body: PdxBody): Integer; cdecl; external ODEDLL;
  function dBodyGetJoint(const Body: PdxBody; const index: Integer): TdJointID; cdecl; external ODEDLL;
  function dBodyGetLinearVel(const Body: PdxBody): PdVector3; cdecl; external ODEDLL;
  procedure dBodyGetMass(const Body: PdxBody; var mass: TdMass); cdecl; external ODEDLL;
  function dBodyGetNumJoints(const Body: PdxBody): Integer; cdecl; external ODEDLL;
  procedure dBodyGetPointVel(const Body: PdxBody; const px, py, pz: TdReal; var result: TdVector3); cdecl; external ODEDLL;
  procedure dBodyGetPosRelPoint(const Body: PdxBody; const px, py, pz: TdReal; var result: TdVector3); cdecl; external ODEDLL;
  function dBodyGetPosition(const Body: PdxBody): PdVector3; cdecl; external ODEDLL;
  function dBodyGetQuaternion(const Body: PdxBody): PdQuaternion; cdecl; external ODEDLL;
  procedure dBodyGetRelPointPos(const Body: PdxBody; const px, py, pz: TdReal; var result: TdVector3); cdecl; external ODEDLL;
  procedure dBodyGetRelPointVel(const Body: PdxBody; const px, py, pz: TdReal; var result: TdVector3); cdecl; external ODEDLL;
  function dBodyGetRotation(const Body: PdxBody): PdMatrix3; cdecl; external ODEDLL;
  function dBodyGetTorque(const Body: PdxBody): PdVector3; cdecl; external ODEDLL;
  function dBodyIsEnabled(const Body: PdxBody): Integer; cdecl; external ODEDLL;
  procedure dBodySetAngularVel(const Body: PdxBody; const x, y, z: TdReal); cdecl; external ODEDLL;
  procedure dBodySetFiniteRotationAxis(const Body: PdxBody; const x, y, z: TdReal); cdecl; external ODEDLL;
  procedure dBodySetFiniteRotationMode(const Body: PdxBody; const mode: Integer); cdecl; external ODEDLL;
  procedure dBodySetForce(const Body: PdxBody; const x, y, z: TdReal); cdecl; external ODEDLL;
  procedure dBodySetGravityMode(const Body: PdxBody; const mode: Integer); cdecl; external ODEDLL;
  procedure dBodySetLinearVel(const Body: PdxBody; const x, y, z: TdReal); cdecl; external ODEDLL;
  procedure dBodySetMass( const Body: PdxBody; const mass: PdMass); cdecl; external ODEDLL;
  procedure dBodySetPosition(const Body: PdxBody; const x, y, z: TdReal); cdecl; external ODEDLL;
  procedure dBodySetQuaternion(const Body: PdxBody; const q: TdQuaternion); cdecl; external ODEDLL;
  procedure dBodySetRotation(const Body: PdxBody; const R: TdMatrix3); cdecl; external ODEDLL;
  procedure dBodySetTorque(const Body: PdxBody; const x, y, z: TdReal); cdecl; external ODEDLL;
  procedure dBodyVectorFromWorld(const Body: PdxBody; const px, py, pz: TdReal; var result: TdVector3); cdecl; external ODEDLL;
  procedure dBodyVectorToWorld(const Body: PdxBody; const px, py, pz: TdReal; var result: TdVector3); cdecl; external ODEDLL;
  procedure dBodySetData (const Body: PdxBody; data : pointer); cdecl; external ODEDLL;
  function dBodyGetData (const Body: PdxBody) : pointer; cdecl; external ODEDLL;
  procedure dBodySetMovedCallback (const Body: PdxBody; Callback: TdMovedCallback); cdecl; external ODEDLL;
  procedure dBodyCopyPosition(const Body: PdxBody; const pos: TdVector3); cdecl; external ODEDLL;
  procedure dBodyCopyRotation(const Body: PdxBody; const R: TdMatrix3); cdecl; external ODEDLL;
  procedure dBodyCopyQuaternion(const Body: PdxBody; const quat: TdQuaternion); cdecl; external ODEDLL;

  // damping functions
  procedure dBodySetLinearDamping (const Body: PdxBody; scale: TdReal); cdecl; external ODEDLL;
  function dBodyGetLinearDamping (const Body: PdxBody): TdReal; cdecl; external ODEDLL;
  procedure dBodySetAngularDamping(const Body: PdxBody; scale : TdReal); cdecl; external ODEDLL;
  function dBodyGetAngularDamping(const Body: PdxBody): TdReal; cdecl; external ODEDLL;
  procedure dBodySetDamping(const Body: PdxBody; linear_scale, angular_scale: TdReal); cdecl; external ODEDLL;
  function dBodyGetLinearDampingThreshold(const Body: PdxBody): TdReal; cdecl; external ODEDLL;
  procedure dBodySetLinearDampingThreshold(const Body: PdxBody; threshold: TdReal); cdecl; external ODEDLL;
  function dBodyGetAngularDampingThreshold(const Body: PdxBody): TdReal; cdecl; external ODEDLL;
  procedure dBodySetAngularDampingThreshold(const Body: PdxBody; threshold: TdReal); cdecl; external ODEDLL;
  procedure dBodySetDampingDefaults(const Body: PdxBody; threshold: TdReal); cdecl; external ODEDLL;
  procedure dBodySetMaxAngularSpeed(const Body: PdxBody; max_speed: TdReal); cdecl; external ODEDLL;
  function dBodyGetMaxAngularSpeed(const Body: PdxBody): TdReal; cdecl; external ODEDLL;

  // Auto-disable functions
  function dBodyGetAutoDisableLinearThreshold(const Body: PdxBody): TdReal; cdecl; external ODEDLL;
  procedure dBodySetAutoDisableLinearThreshold(const Body: PdxBody; linThreshold: TdReal); cdecl; external ODEDLL;
  function dBodyGetAutoDisableAngularThreshold(const Body: PdxBody): TdReal; cdecl; external ODEDLL;
  procedure dBodySetAutoDisableAngularThreshold(const Body: PdxBody; angThreshold: TdReal); cdecl; external ODEDLL;
  function dBodyGetAutoDisableSteps(const Body: PdxBody): Integer; cdecl; external ODEDLL;
  procedure dBodySetAutoDisableSteps(const Body: PdxBody; steps: Integer); cdecl; external ODEDLL;
  function dBodyGetAutoDisableTime(const Body: PdxBody): TdReal; cdecl; external ODEDLL;
  procedure dBodySetAutoDisableTime(const Body: PdxBody; time: TdReal); cdecl; external ODEDLL;
  function dBodyGetAutoDisableFlag(const Body: PdxBody): Integer; cdecl; external ODEDLL;
  procedure dBodySetAutoDisableFlag(const Body: PdxBody; do_auto_disable: Integer); cdecl; external ODEDLL;
  procedure dBodySetAutoDisableDefaults(const Body: PdxBody); cdecl; external ODEDLL;
  procedure dBodySetAutoDisableAverageSamplesCount(const Body: PdxBody; average_samples_count: longword); cdecl; external ODEDLL;


  //----- dJoint -----
  {$IFDEF PARODE}
  // breakable joints
  procedure dJointSetBreakable (const dJointID: TdJointID; b: integer); cdecl; external ODEDLL;
  procedure dJointSetBreakCallback (const dJointID: TdJointID; callbackFunc:TdJointBreakCallback); cdecl; external ODEDLL;
  procedure dJointSetBreakMode (const dJointID: TdJointID; mode: integer); cdecl; external ODEDLL;
  function dJointGetBreakMode (const dJointID: TdJointID): integer; cdecl; external ODEDLL;
  procedure dJointSetBreakForce(const dJointID: TdJointID; body: integer; x, y, z: TdReal); cdecl; external ODEDLL;
  procedure dJointSetBreakTorque(const dJointID: TdJointID; body: integer; x, y, z: TdReal); cdecl; external ODEDLL;
  function dJointIsBreakable (const dJointID: TdJointID): integer; cdecl; external ODEDLL;
  procedure dJointGetBreakForce(const dJointID: TdJointID; body: integer; var force: TdVector3); cdecl; external ODEDLL;
  procedure dJointGetBreakTorque(const dJointID: TdJointID; body: integer; var torque: TdVector3); cdecl; external ODEDLL;
  {$ENDIF}

  // normal joints
  procedure dJointGroupDestroy(const dJointGroupID: TdJointGroupID); cdecl; external ODEDLL;
  function dJointGroupCreate(const max_size: Integer): TdJointGroupID; cdecl; external ODEDLL;
  procedure dJointGroupEmpty(const dJointGroupID: TdJointGroupID); cdecl; external ODEDLL;

  procedure dJointAttach(const dJointID: TdJointID; const body1, body2: PdxBody); cdecl; external ODEDLL;
  procedure dJointDestroy(const dJointID: TdJointID); cdecl; external ODEDLL;

  function dJointGetData (const dJointID: TdJointID): pointer; cdecl; external ODEDLL;
  procedure dJointSetData (const dJointID: TdJointID; data: Pointer); cdecl; external ODEDLL;

  // callback routines for feedback of joints
  procedure dJointSetFeedback (const dJointID: TdJointID; Feedback: PTdJointFeedback); cdecl; external ODEDLL;
  function dJointGetFeedback (const dJointID: TdJointID): PTdJointFeedback; cdecl; external ODEDLL;

  function dJointGetType(const dJointID: TdJointID): Integer; cdecl; external ODEDLL;
  function dJointGetBody(const dJointID: TdJointID; const index: Integer): PdxBody; cdecl; external ODEDLL;

  //Contact
  function dJointCreateContact(const World: PdxWorld; dJointGroupID: TdJointGroupID; const dContact: PdContact): TdJointID; cdecl; external ODEDLL;

  //AMotor
  function dJointCreateAMotor(const World: PdxWorld; dJointGroupID: TdJointGroupID): TdJointID; cdecl; external ODEDLL;
  procedure dJointSetAMotorAngle(const dJointID: TdJointID; const anum: Integer; const angle: TdReal); cdecl; external ODEDLL;
  function dJointGetAMotorAngle(const dJointID: TdJointID; const anum: Integer): TdReal; cdecl; external ODEDLL;
  procedure dJointSetAMotorAxis(const dJointID: TdJointID; const anum, rel: Integer; const x, y, z: TdReal); cdecl; external ODEDLL;
  procedure dJointGetAMotorAxis(const dJointID: TdJointID; const anum: Integer; var result: TdVector3); cdecl; external ODEDLL;
  procedure dJointSetAMotorNumAxes(const dJointID: TdJointID; const num: Integer); cdecl; external ODEDLL;
  function dJointGetAMotorNumAxes(const dJointID: TdJointID): Integer; cdecl; external ODEDLL;
  procedure dJointSetAMotorParam(const dJointID: TdJointID; const parameter: TJointParams; const value: TdReal); cdecl; external ODEDLL;
  function dJointGetAMotorParam(const dJointID: TdJointID; const parameter: TJointParams): TdReal; cdecl; external ODEDLL;
  procedure dJointSetAMotorMode(const dJointID: TdJointID; const mode: TdAngularMotorModeNumbers); cdecl; external ODEDLL;
  function dJointGetAMotorMode(const dJointID: TdJointID): Integer; cdecl; external ODEDLL;
  procedure dJointAddAMotorTorques (const dJointID: TdJointID; torque1, torque2, torque3: TdReal); cdecl; external ODEDLL;
  function dJointGetAMotorAngleRate(const dJointID: TdJointID; const anum: Integer): TdReal; cdecl; external ODEDLL;
  function dJointGetAMotorAxisRel(const dJointID: TdJointID; const anum: Integer): Integer; cdecl; external ODEDLL;

  //LMotor
  function dJointCreateLMotor(const World: PdxWorld; dJointGroupID : TdJointGroupID): TdJointID; cdecl; external ODEDLL;
  procedure dJointSetLMotorAxis(const dJointID: TdJointID; const anum, rel: Integer; const x, y, z: TdReal); cdecl; external ODEDLL;
  procedure dJointGetLMotorAxis(const dJointID: TdJointID; const anum: Integer; var result: TdVector3); cdecl; external ODEDLL;
  procedure dJointSetLMotorNumAxes(const dJointID: TdJointID; const num: Integer); cdecl; external ODEDLL;
  function dJointGetLMotorNumAxes(const dJointID: TdJointID): Integer; cdecl; external ODEDLL;
  procedure dJointSetLMotorParam(const dJointID: TdJointID; const parameter: TJointParams; const value: TdReal); cdecl; external ODEDLL;
  function dJointGetLMotorParam(const dJointID: TdJointID; const parameter: TJointParams): TdReal; cdecl; external ODEDLL;

  //Ball
  function dJointCreateBall(const World: PdxWorld; dJointGroupID : TdJointGroupID): TdJointID; cdecl; external ODEDLL;
  procedure dJointSetBallAnchor(const dJointID: TdJointID; const x, y, z: TdReal); cdecl; external ODEDLL;
  procedure dJointGetBallAnchor(const dJointID: TdJointID; var result: TdVector3); cdecl; external ODEDLL;
  procedure dJointGetBallAnchor2(const dJointID: TdJointID; var result: TdVector3); cdecl; external ODEDLL;
  procedure dJointSetBallParam(const dJointID : TdJointID; const parameter: TJointParams; const value: TdReal); cdecl; external {$IFDEF __GPC__}name 'dJointSetBallParam'{$ELSE} ODEDLL{$ENDIF __GPC__};

  //Hinge
  function dJointCreateHinge(const World: PdxWorld; dJointGroupID : TdJointGroupID): TdJointID; cdecl; external ODEDLL;
  procedure dJointSetHingeAnchor(const dJointID: TdJointID; const x, y, z: TdReal); cdecl; external ODEDLL;
  procedure dJointGetHingeAnchor(const dJointID: TdJointID; var result: TdVector3); cdecl; external ODEDLL;
  procedure dJointGetHingeAnchor2(const dJointID: TdJointID; var result: TdVector3); cdecl; external ODEDLL;
  procedure dJointSetHingeAxis(const dJointID: TdJointID; const x, y, z: TdReal); cdecl; external ODEDLL;
  procedure dJointGetHingeAxis(const dJointID: TdJointID; var result: TdVector3); cdecl; external ODEDLL;
  procedure dJointSetHingeParam(const dJointID: TdJointID; const parameter: TJointParams; const value: TdReal); cdecl; external ODEDLL;
  function dJointGetHingeParam(const dJointID: TdJointID; const parameter: TJointParams): TdReal; cdecl; external ODEDLL;
  function dJointGetHingeAngle(const dJointID: TdJointID): TdReal; cdecl; external ODEDLL;
  function dJointGetHingeAngleRate(const dJointID: TdJointID): TdReal; cdecl; external ODEDLL;
  procedure dJointAddHingeTorque (const dJointID: TdJointID; torque: TdReal); cdecl; external ODEDLL;

  //Hinge2
  function dJointCreateHinge2(const World: PdxWorld; dJointGroupID : TdJointGroupID): TdJointID; cdecl; external ODEDLL;
  procedure dJointSetHinge2Anchor(const dJointID: TdJointID; const x, y, z: TdReal); cdecl; external ODEDLL;
  procedure dJointGetHinge2Anchor(const dJointID: TdJointID; var result: TdVector3); cdecl; external ODEDLL;
  procedure dJointGetHinge2Anchor2(const dJointID: TdJointID; var result: TdVector3); cdecl; external ODEDLL;

  //deprecated
  //procedure dJointSetHinge2Axis1(const dJointID: TdJointID; const x, y, z: TdReal); cdecl; external ODEDLL;
  procedure dJointGetHinge2Axis1(const dJointID: TdJointID; var result: TdVector3); cdecl; external ODEDLL;
  //procedure dJointSetHinge2Axis2(const dJointID: TdJointID; const x, y, z: TdReal); cdecl; external ODEDLL;
  procedure dJointGetHinge2Axis2(const dJointID: TdJointID; var result: TdVector3); cdecl; external ODEDLL;

  procedure dJointSetHinge2Param(const dJointID: TdJointID; const parameter: TJointParams; const value: TdReal); cdecl; external ODEDLL;
  function dJointGetHinge2Param(const dJointID: TdJointID; const parameter: TJointParams): TdReal; cdecl; external ODEDLL;
  function dJointGetHinge2Angle1(const dJointID: TdJointID): TdReal; cdecl; external ODEDLL;
  function dJointGetHinge2Angle1Rate(const dJointID: TdJointID): TdReal; cdecl; external ODEDLL;
  function dJointGetHinge2Angle2Rate(const dJointID: TdJointID): TdReal; cdecl; external ODEDLL;
  procedure dJointAddHinge2Torques (const dJointID: TdJointID; torque1, torque2: TdReal); cdecl; external ODEDLL;
  procedure dJointCorrectHinge2(const dJointID: TdJointID); cdecl; external ODEDLL;

  //Slider
  function dJointCreateSlider(const World: PdxWorld; dJointGroupID : TdJointGroupID): TdJointID; cdecl; external ODEDLL;
  procedure dJointSetSliderAxis(const dJointID: TdJointID; const x, y, z: TdReal); cdecl; external ODEDLL;
  procedure dJointGetSliderAxis(const dJointID: TdJointID; var result: TdVector3); cdecl; external ODEDLL;
  procedure dJointSetSliderParam(const dJointID: TdJointID; const parameter: TJointParams; const value: TdReal); cdecl; external ODEDLL;
  function dJointGetSliderParam(const dJointID: TdJointID; const parameter: TJointParams): TdReal; cdecl; external ODEDLL;
  function dJointGetSliderPosition(const dJointID: TdJointID): TdReal; cdecl; external ODEDLL;
  function dJointGetSliderPositionRate(const dJointID: TdJointID): TdReal; cdecl; external ODEDLL;
  procedure dJointAddSliderForce (const dJointID: TdJointID; force: TdReal); cdecl; external ODEDLL;

  //Universal
  function dJointCreateUniversal(const World: PdxWorld; dJointGroupID : TdJointGroupID): TdJointID; cdecl; external ODEDLL;
  procedure dJointGetUniversalAnchor(const dJointID: TdJointID; var result: TdVector3); cdecl; external ODEDLL;
  procedure dJointGetUniversalAnchor2(const dJointID: TdJointID; var result: TdVector3); cdecl; external ODEDLL;
  procedure dJointSetUniversalAxis1(const dJointID: TdJointID; const x, y, z: TdReal); cdecl; external ODEDLL;
  procedure dJointGetUniversalAxis1(const dJointID: TdJointID; var result: TdVector3); cdecl; external ODEDLL;
  procedure dJointSetUniversalAxis2(const dJointID: TdJointID; const x, y, z: TdReal); cdecl; external ODEDLL;
  procedure dJointGetUniversalAxis2(const dJointID: TdJointID; var result: TdVector3); cdecl; external ODEDLL;
  procedure dJointSetUniversalParam(const dJointID: TdJointID; const parameter: TJointParams; const value: TdReal); cdecl; external ODEDLL;
  function dJointGetUniversalParam(const dJointID: TdJointID; const parameter: TJointParams): TdReal; cdecl; external ODEDLL;
  function dJointGetUniversalAngle1(const dJointID: TdJointID): TdReal; cdecl; external ODEDLL;
  function dJointGetUniversalAngle2(const dJointID: TdJointID): TdReal; cdecl; external ODEDLL;
  function dJointGetUniversalAngle1Rate(const dJointID: TdJointID): TdReal; cdecl; external ODEDLL;
  function dJointGetUniversalAngle2Rate(const dJointID: TdJointID): TdReal; cdecl; external ODEDLL;
  procedure dJointSetUniversalAnchor(const dJointID: TdJointID; const x, y, z: TdReal); cdecl; external ODEDLL;
  procedure dJointAddUniversalTorques (const dJointID: TdJointID; torque1, torque2: TdReal); cdecl; external ODEDLL;

  //Fixed
  function dJointCreateFixed(const World: PdxWorld; dJointGroupID : TdJointGroupID): TdJointID; cdecl; external ODEDLL;
  procedure dJointSetFixed(const dJointID: TdJointID); cdecl; external ODEDLL;

  //Plane2D
  function dJointCreatePlane2D(const World: PdxWorld; dJointGroupID : TdJointGroupID): TdJointID; cdecl; external ODEDLL;
  procedure dJointSetPlane2DXParam(const dJointID: TdJointID; const parameter: Integer; const value: TdReal); cdecl; external ODEDLL;
  procedure dJointSetPlane2DYParam(const dJointID: TdJointID; const parameter: Integer; const value: TdReal); cdecl; external ODEDLL;
  procedure dJointSetPlane2DAngleParam(const dJointID: TdJointID; const parameter: Integer; const value: TdReal); cdecl; external ODEDLL;

  //PR
  function dJointCreatePR(const World: PdxWorld; dJointGroupID : TdJointGroupID): TdJointID; cdecl; external ODEDLL;
  procedure dJointSetPRAnchor(const dJointID: TdJointID; const x, y, z: TdReal); cdecl; external ODEDLL;
  procedure dJointSetPRAxis1(const dJointID: TdJointID; const x, y, z: TdReal); cdecl; external ODEDLL;
  procedure dJointGetPRAxis1(const dJointID: TdJointID; var result: TdVector3); cdecl; external ODEDLL;
  procedure dJointSetPRAxis2(const dJointID: TdJointID; const x, y, z: TdReal); cdecl; external ODEDLL;
  procedure dJointGetPRAxis2(const dJointID: TdJointID; var result: TdVector3); cdecl; external ODEDLL;
  procedure dJointSetPRParam(const dJointID: TdJointID; const parameter: TJointParams; const value: TdReal); cdecl; external ODEDLL;
  function dJointGetPRParam(const dJointID: TdJointID; parameter: integer): TdReal; cdecl; external ODEDLL;
  procedure dJointAddPRTorque (const dJointID: TdJointID; torque: TdReal); cdecl; external ODEDLL;

  //Piston
  function dJointCreatePiston(const World: PdxWorld; dJointGroupID : TdJointGroupID): TdJointID; cdecl; external ODEDLL;
  procedure dJointSetPistonAnchor(const dJointID: TdJointID; const x, y, z: TdReal); cdecl; external ODEDLL;
  procedure dJointGetPistonAnchor(const dJointID: TdJointID; var result: TdVector3); cdecl; external ODEDLL;
  procedure dJointGetPistonAnchor2(const dJointID: TdJointID; var result: TdVector3); cdecl; external ODEDLL;
  procedure dJointSetPistonAxis(const dJointID: TdJointID; const x, y, z: TdReal); cdecl; external ODEDLL;
  procedure dJointGetPistonAxis(const dJointID: TdJointID; var result: TdVector3); cdecl; external ODEDLL;
  procedure dJointSetPistonParam(const dJointID: TdJointID; const parameter: TJointParams; const value: TdReal); cdecl; external ODEDLL;
  function dJointGetPistonParam(const dJointID: TdJointID; parameter : integer): TdReal; cdecl; external ODEDLL;
  procedure dJointSetPistonAxisDelta(const dJointID: TdJointID; const x, y, z, ax, ay, az: TdReal); cdecl; external ODEDLL;
  procedure dJointAddPistonForce (const dJointID: TdJointID; force: TdReal); cdecl; external ODEDLL;
  function dJointGetPistonPosition(const dJointID: TdJointID): TdReal; cdecl; external ODEDLL;
  function dJointGetPistonAngle(const dJointID: TdJointID): TdReal; cdecl; external ODEDLL;
  function dJointGetPistonAngleRate(const dJointID: TdJointID): TdReal; cdecl; external ODEDLL;
  function dJointGetPistonRate(const dJointID: TdJointID): TdReal; cdecl; external ODEDLL;
  
  //Transmission
  function dJointCreateTransmission(const World: PdxWorld; dJointGroupID : TdJointGroupID): TdJointID; cdecl; external ODEDLL;
  procedure dJointGetTransmissionContactPoint1(const dJointID: TdJointID; var result: TdVector3); cdecl; external ODEDLL;
  procedure dJointGetTransmissionContactPoint2(const dJointID: TdJointID; var result: TdVector3); cdecl; external ODEDLL;
  procedure dJointSetTransmissionAxis1(const dJointID: TdJointID; const x, y, z: TdReal); cdecl; external ODEDLL;
  procedure dJointGetTransmissionAxis1(const dJointID: TdJointID; var result: TdVector3); cdecl; external ODEDLL;
  procedure dJointSetTransmissionAxis2(const dJointID: TdJointID; const x, y, z: TdReal); cdecl; external ODEDLL;
  procedure dJointGetTransmissionAxis2(const dJointID: TdJointID; var result: TdVector3); cdecl; external ODEDLL;
  procedure dJointSetTransmissionAnchor1(const dJointID: TdJointID; const x, y, z: TdReal); cdecl; external ODEDLL;
  procedure dJointGetTransmissionAnchor1(const dJointID: TdJointID; var result: TdVector3); cdecl; external ODEDLL;
  procedure dJointSetTransmissionAnchor2(const dJointID: TdJointID; const x, y, z: TdReal); cdecl; external ODEDLL;
  procedure dJointGetTransmissionAnchor2(const dJointID: TdJointID; var result: TdVector3); cdecl; external ODEDLL;
  procedure dJointSetTransmissionParam(const dJointID: TdJointID; const parameter: integer; const value: TdReal); cdecl; external ODEDLL;
  function dJointGetTransmissionParam(const dJointID: TdJointID; parameter : integer): TdReal; cdecl; external ODEDLL;
  function dJointSetTransmissionMode(const dJointID: TdJointID; mode : integer): TdReal; cdecl; external ODEDLL;
  function dJointGetTransmissionMode(const dJointID: TdJointID): integer; cdecl; external ODEDLL;
  procedure dJointSetTransmissionRatio(const dJointID: TdJointID; const ratio: TdReal); cdecl; external ODEDLL;
  function dJointGetTransmissionRatio(const dJointID: TdJointID): TdReal; cdecl; external ODEDLL;
  procedure dJointSetTransmissionAxis(const dJointID: TdJointID; const x, y, z: TdReal); cdecl; external ODEDLL;
  procedure dJointGetTransmissionAxis(const dJointID: TdJointID; var result: TdVector3); cdecl; external ODEDLL;
  function dJointGetTransmissionAngle1(const dJointID: TdJointID): TdReal; cdecl; external ODEDLL;
  function dJointGetTransmissionAngle2(const dJointID: TdJointID): TdReal; cdecl; external ODEDLL;
  function dJointGetTransmissionRadius1(const dJointID: TdJointID): TdReal; cdecl; external ODEDLL;
  function dJointGetTransmissionRadius2(const dJointID: TdJointID): TdReal; cdecl; external ODEDLL;
  procedure dJointSetTransmissionRadius1(const dJointID: TdJointID; radius: TdReal); cdecl; external ODEDLL;
  procedure dJointSetTransmissionRadius2(const dJointID: TdJointID; radius: TdReal); cdecl; external ODEDLL;
  function dJointGetTransmissionBacklash(const dJointID: TdJointID): TdReal; cdecl; external ODEDLL;
  procedure dJointSetTransmissionBacklash(const dJointID: TdJointID; backlash: TdReal); cdecl; external ODEDLL;

  //----- dGeom -----
  function dCreateGeom (classnum: Integer): PdxGeom; cdecl; external ODEDLL;
  procedure dGeomDestroy(const Geom: PdxGeom); cdecl; external ODEDLL;

  function dCreateGeomClass(const classptr: TdGeomClass): Integer; cdecl; external ODEDLL;
  function dGeomGetClass(const Geom: PdxGeom): Integer; cdecl; external ODEDLL;
  function dGeomGetClassData(o: PdxGeom): Pointer; cdecl; external ODEDLL;

  function dGeomGetSpace(const Geom: PdxGeom): PdxSpace; cdecl; external ODEDLL;
  function dGeomIsSpace (const Geom: PdxGeom): integer; cdecl; external ODEDLL;
  procedure dGeomSetBody(const Geom: PdxGeom; Body: PdxBody); cdecl; external ODEDLL;
  function dGeomGetBody(const Geom: PdxGeom): PdxBody; cdecl; external ODEDLL;

  procedure dGeomSetPosition(const Geom: PdxGeom; const x, y, z: TdReal); cdecl; external ODEDLL;
  function dGeomGetPosition(const Geom: PdxGeom): PdVector3; cdecl; external ODEDLL;
  procedure dGeomSetRotation(const Geom: PdxGeom; const R: TdMatrix3); cdecl; external ODEDLL;
  function dGeomGetRotation(const Geom: PdxGeom): PdMatrix3; cdecl; external ODEDLL;
  procedure dGeomSetQuaternion(const Geom: PdxGeom; const TdQuaternion); cdecl; external ODEDLL;
  procedure dGeomGetQuaternion(const Geom: PdxGeom; var result: TdQuaternion); cdecl; external ODEDLL;
  procedure dGeomCopyPosition(const Geom: PdxGeom; const pos: TdVector3); cdecl; external ODEDLL;
  procedure dGeomCopyRotation(const Geom: PdxGeom; const R: TdMatrix3); cdecl; external ODEDLL;
  procedure dGeomCopyQuaternion(const Geom: PdxGeom; const quat: TdQuaternion); cdecl; external ODEDLL;

  procedure dGeomSetData (const Geom: PdxGeom; data : pointer); cdecl; external ODEDLL;
  function dGeomGetData (const Geom: PdxGeom): pointer; cdecl; external ODEDLL;
  procedure dGeomEnable (const Geom: PdxGeom); cdecl; external ODEDLL;
  procedure dGeomDisable (const Geom: PdxGeom); cdecl; external ODEDLL;
  function dGeomIsEnabled (const Geom: PdxGeom): integer; cdecl; external ODEDLL;
  procedure dGeomGetAABB(const Geom: PdxGeom; var aabb: TdAABB); cdecl; external ODEDLL;
  procedure dGeomSetCategoryBits (const Geom: PdxGeom; bits : Cardinal); cdecl; external ODEDLL;
  function dGeomGetCategoryBits (const Geom: PdxGeom): cardinal; cdecl; external ODEDLL;
  procedure dGeomSetCollideBits (const Geom: PdxGeom; bits : Cardinal); cdecl; external ODEDLL;
  function dGeomGetCollideBits (const Geom: PdxGeom): cardinal; cdecl; external ODEDLL;

  procedure dGeomSetOffsetPosition (const Geom: PdxGeom; x,y,z:TdReal); cdecl; external ODEDLL;
  function dGeomGetOffsetPosition(const Geom: PdxGeom): PdVector3; cdecl; external ODEDLL;
  procedure dGeomSetOffsetRotation (const Geom: PdxGeom; R:TdMatrix3); cdecl; external ODEDLL;
  function dGeomGetOffsetRotation(const Geom: PdxGeom): PdVector3; cdecl; external ODEDLL;
  procedure dGeomSetOffsetQuaternion (const Geom: PdxGeom; const Q:TdQuaternion); cdecl; external ODEDLL;
  procedure dGeomGetOffsetQuaternion (const Geom: PdxGeom; var Q:TdQuaternion); cdecl; external ODEDLL;
  procedure dGeomClearOffset (const Geom: PdxGeom); cdecl; external ODEDLL;
  procedure dGeomSetOffsetWorldPosition (const Geom: PdxGeom; x,y,z:TdReal); cdecl; external ODEDLL;
  procedure dGeomSetOffsetWorldRotation (const Geom: PdxGeom; R:TdMatrix3); cdecl; external ODEDLL;
  procedure dGeomSetOffsetWorldQuaternion (const Geom: PdxGeom; const Q:TdQuaternion); cdecl; external ODEDLL;
  procedure dGeomCopyOffsetPosition(const Geom: PdxGeom; var pos: TdVector3); cdecl; external ODEDLL;
  procedure dGeomCopyOffsetRotation(const Geom: PdxGeom; var R: TdMatrix3); cdecl; external ODEDLL;
  procedure dGeomIsOffset(const Geom: PdxGeom); cdecl; external ODEDLL;

  //Transform
  //function dCreateGeomTransform(const Space: PdxSpace): PdxGeom; cdecl; external ODEDLL;
  //procedure dGeomTransformSetGeom(const Geom, obj: PdxGeom); cdecl; external ODEDLL;
  //deprecated
  function dGeomTransformGetGeom(const Geom: PdxGeom): PdxGeom; cdecl; external ODEDLL;
  //procedure dGeomTransformSetInfo(const Geom: PdxGeom; mode: integer); cdecl; external ODEDLL;
  //function dGeomTransformGetInfo(const Geom: PdxGeom): integer; cdecl; external ODEDLL;
  //procedure dGeomTransformSetCleanup(const Geom: PdxGeom; const mode: Integer); cdecl; external ODEDLL;
  //function dGeomTransformGetCleanup(const Geom: PdxGeom): Integer; cdecl; external ODEDLL;

  //Box
  function dCreateBox(const Space: PdxSpace; const lx, ly, lz: TdReal): PdxGeom; cdecl; external ODEDLL;
  procedure dGeomBoxGetLengths(const Geom: PdxGeom; var result: TdVector3); cdecl; external ODEDLL;
  procedure dGeomBoxSetLengths(const Geom: PdxGeom; const lx, ly, lz: TdReal); cdecl; external ODEDLL;
  function dGeomBoxPointDepth(const Geom: PdxGeom; const x,y,z: TdReal): TdReal; cdecl; external ODEDLL;

  // dCylinder (not a capped cylinder).
  function dCreateCylinder(const Space: PdxSpace; r, lz: TdReal): PdxGeom; cdecl; external ODEDLL;
  procedure dGeomCylinderSetParams(const Geom: PdxGeom; radius, length: TdReal); cdecl; external ODEDLL;
  procedure dGeomCylinderGetParams(const Geom: PdxGeom; var radius, length: TdReal); cdecl; external ODEDLL;

  // dCapsule (a capped cylinder).
  function dCreateCapsule(const Space: PdxSpace; const radius, length: TdReal): PdxGeom; cdecl; external ODEDLL;
  procedure dGeomCapsuleSetParams(const Geom: PdxGeom; const radius, length: TdReal); cdecl; external ODEDLL;
  procedure dGeomCapsuleGetParams(const Geom: PdxGeom; var radius, length: TdReal); cdecl; external ODEDLL;
  function dGeomCapsulePointDepth(const Geom: PdxGeom; const x,y,z: TdReal): TdReal; cdecl; external ODEDLL;

  //Plane
  function dCreatePlane(const Space: PdxSpace; const a, b, c, d: TdReal): PdxGeom; cdecl; external ODEDLL;
  procedure dGeomPlaneSetParams(const Geom: PdxGeom; const a, b, c, d: TdReal); cdecl; external ODEDLL;
  procedure dGeomPlaneGetParams(const Geom: PdxGeom; var result: TdVector4); cdecl; external ODEDLL;
  function dGeomPlanePointDepth(const Geom: PdxGeom; const x,y,z: TdReal): TdReal; cdecl; external ODEDLL;

  //Sphere
  function dCreateSphere(const Space: PdxSpace; const radius: TdReal): PdxGeom; cdecl; external ODEDLL;
  procedure dGeomSphereSetRadius(const Geom: PdxGeom; const radius: TdReal); cdecl; external ODEDLL;
  function dGeomSphereGetRadius(const Geom: PdxGeom): TdReal; cdecl; external ODEDLL;
  function dGeomSpherePointDepth(const Geom: PdxGeom; const x,y,z: TdReal): TdReal; cdecl; external ODEDLL;

  //Convex
  function dCreateConvex(const Space: PdxSpace; _planes: PdReal; _planecount: longword; _points: PdReal; _pointcount: longword; const _polygons:longword): PdxGeom; cdecl; external ODEDLL;
  procedure dGeomSetConvex(const Geom: PdxGeom; _planes: PdReal; _planecount: longword; _points: PdReal; _pointcount: longword; const _polygons:longword); cdecl; external ODEDLL;

  //Heightfield  (incomplete)
  function dCreateHeightfield(const Space: PdxSpace; Data: PdxHeightfieldData; bPlaceable:Integer): PdxGeom; cdecl; external ODEDLL;
  function dGeomHeightfieldDataCreate: PdxHeightfieldData; cdecl; external ODEDLL;
  procedure dGeomHeightfieldDataDestroy(Data: PdxHeightfieldData); cdecl; external ODEDLL;
  procedure dGeomHeightfieldSetHeightfieldData(const Geom: PdxGeom; Data: PdxHeightfieldData); cdecl; external ODEDLL;
  function dGeomHeightfieldGetHeightfieldData(const Geom: PdxGeom):PdxHeightfieldData; cdecl; external ODEDLL;
  function dGeomHeightfieldDataSetBounds(Data: PdxHeightfieldData; minHeight, MaxHeight: TdReal) : TdReal; cdecl; external ODEDLL;

  //dRay
  function dCreateRay(const Space: PdxSpace; length: TdReal): PdxGeom; cdecl; external ODEDLL;
  procedure dGeomRaySet(const geom: PdxGeom; px, py, pz, dx, dy, dz: TdReal); cdecl; external ODEDLL;
  procedure dGeomRayGet(const geom: PdxGeom; var start, dir: TdVector3); cdecl; external ODEDLL;
  procedure dGeomRaySetLength(const geom: PdxGeom; length: TdReal); cdecl; external ODEDLL;
  function dGeomRayGetLength(const geom: PdxGeom): TdReal; cdecl; external ODEDLL;
  procedure dGeomRaySetParams(const geom: PdxGeom; FirstContact, BackfacCull: Integer); cdecl; external ODEDLL;
  procedure dGeomRayGetParams(const geom: PdxGeom; var FirstContact, BackfacCull: Integer); cdecl; external ODEDLL;
  procedure dGeomRaySetClosestHit(const geom: PdxGeom; closestHit: Integer); cdecl; external ODEDLL;
  function dGeomRayGetClosestHit(const geom: PdxGeom): Integer; cdecl; external ODEDLL;

  //TriMesh
  function dCreateTriMesh(const Space : PdxSpace; Data: PdxTriMeshData; Callback:TdTriCallback; ArrayCallback:TdTriArrayCallback; RayCallback: TdTriRayCallback): PdxGeom; cdecl; external ODEDLL;

  procedure dGeomTriMeshSetData(g: PdxGeom; Data: PdxTriMeshData); cdecl; external ODEDLL;
  function dGeomTriMeshGetData(g: PdxGeom):PdxTriMeshData; cdecl; external ODEDLL;
  function dGeomTriMeshGetTriMeshDataID(g: PdxGeom): PdxTriMeshData; cdecl; external ODEDLL;
  procedure dGeomTriMeshDataUpdate(g: PdxTriMeshData); cdecl; external ODEDLL;

  function dGeomTriMeshIsTCEnabled(g: PdxGeom; geomClass: Integer): Integer; cdecl; external ODEDLL;
  procedure dGeomTriMeshEnableTC(g: PdxGeom; geomClass, enable: Integer); cdecl; external ODEDLL;
  procedure dGeomTriMeshClearTCCache(g: PdxGeom); cdecl; external ODEDLL;

  function dGeomTriMeshGetTriangleCount(g: PdxGeom) : integer; cdecl; external ODEDLL;
  procedure dGeomTriMeshGetTriangle(g: PdxGeom; Index: Integer; v0, v1, v2: PdVector3); cdecl; external ODEDLL;
  procedure dGeomTriMeshGetPoint(g: PdxGeom; Index: Integer; u, v: TdReal; result: TdVector3); cdecl; external ODEDLL;
  function dGeomTriMeshGetArrayCallback(g: PdxGeom): Pointer; cdecl; external ODEDLL;
  function dGeomTriMeshGetRayCallback(g: PdxGeom): Pointer; cdecl; external ODEDLL;
  procedure dGeomTriMeshSetArrayCallback(g: PdxGeom; ArrayCallback: Pointer); cdecl; external ODEDLL;
  procedure dGeomTriMeshSetRayCallback(g: PdxGeom; RayCallback: Pointer); cdecl; external ODEDLL;
  procedure dGeomTriMeshSetCallback(g: PdxGeom; Callback: Pointer); cdecl; external ODEDLL;
  function dGeomTriMeshGetCallback(g: PdxGeom): Pointer; cdecl; external ODEDLL;

  procedure dGeomTriMeshDataDestroy(g: PdxTriMeshData); cdecl; external ODEDLL;
  function dGeomTriMeshDataCreate: PdxTriMeshData; cdecl; external ODEDLL;
  procedure dGeomTriMeshDataSet(g: PdxTriMeshData; data_id: Integer; data: Pointer); cdecl; external ODEDLL;

  procedure dGeomTriMeshDataBuildSimple(g: PdxTriMeshData; Vertices: PdVector3Array; VertexCount: Integer; Indices: PdIntegerArray; IndexCount: Integer); cdecl; external ODEDLL;
  procedure dGeomTriMeshDataBuildSimple1(g: PdxTriMeshData; Vertices: PdVector3Array; VertexCount: Integer; Indices: PdIntegerArray; IndexCount: Integer; Normals: PdVector3Array); cdecl; external ODEDLL;
  procedure dGeomTriMeshDataBuildDouble(g: PdxTriMeshData; Vertices: PdVector3Array; VertexStride, VertexCount: Integer; Indices: PdIntegerArray; IndexCount, TriStride: Integer); cdecl; external ODEDLL;
  procedure dGeomTriMeshDataBuildDouble1(g: PdxTriMeshData; Vertices: PdVector3Array; VertexStride, VertexCount: Integer; Indices: PdIntegerArray; IndexCount, TriStride: Integer; Normals: PdVector3Array); cdecl; external ODEDLL;
  procedure dGeomTriMeshDataBuildSingle(g: PdxTriMeshData; Vertices: PdVector3Array; VertexStride, VertexCount: Integer; Indices: PdIntegerArray; IndexCount, TriStride: Integer); cdecl; external ODEDLL;
  procedure dGeomTriMeshDataBuildSingle1(g: PdxTriMeshData; Vertices: PdVector3Array; VertexStride, VertexCount: Integer; Indices: PdIntegerArray; IndexCount, TriStride: Integer; Normals: PdVector3Array); cdecl; external ODEDLL;

  procedure dInfiniteAABB(geom: PdxGeom; var aabb: TdAABB); cdecl; external ODEDLL;

  //----- dSpace -----
  procedure dSpaceDestroy(const Space: PdxSpace); cdecl; external ODEDLL;

  function dSimpleSpaceCreate(Space: PdxSpace): PdxSpace; cdecl; external ODEDLL;
  function dHashSpaceCreate(Space: PdxSpace): PdxSpace; cdecl; external ODEDLL;
  function dQuadTreeSpaceCreate(const Space: PdxSpace; const Center, Extents : TdVector3; const Depth : Integer): PdxSpace; cdecl; external ODEDLL;

  procedure dSpaceAdd(const Space: PdxSpace; const Geom: PdxGeom); cdecl; external ODEDLL;
  procedure dSpaceRemove(const Space: PdxSpace; const Geom: PdxGeom); cdecl; external ODEDLL;
  procedure dSpaceClean (const Space: PdxSpace); cdecl; external ODEDLL;
  function dSpaceQuery (const Space: PdxSpace; const Geom: PdxGeom): Integer; cdecl; external ODEDLL;
  function dSpaceGetNumGeoms (const Space: PdxSpace): integer; cdecl; external ODEDLL;
  function dSpaceGetGeom(const Space: PdxSpace; const i: Integer): PdxGeom; cdecl; external ODEDLL;
  procedure dHashSpaceSetLevels(const Space: PdxSpace; const minlevel, maxlevel: Integer); cdecl; external ODEDLL;
  procedure dHashSpaceGetLevels(const Space: PdxSpace; var minlevel, maxlevel: Integer); cdecl; external ODEDLL;
  procedure dSpaceSetCleanup (space: PdxSpace; const mode: integer); cdecl; external ODEDLL;
  function dSpaceGetCleanup(Space: PdxSpace): integer; cdecl; external ODEDLL;

  function dCollide(o1, o2: PdxGeom; flags: integer; var Contact: TdContactGeom; Skip: integer) : integer; cdecl; external ODEDLL;
  procedure dSpaceCollide(const Space : PdxSpace; data: pointer; callback: TdNearCallback); cdecl; external ODEDLL;
  procedure dSpaceCollide2(o1, o2: PdxGeom; data: pointer; callback: TdNearCallback); cdecl; external ODEDLL;

  //----- dMass -----
  procedure dMassSetParameters(var m: TdMass; themass, cgx, cgy, cgz, I11, I22, I33, I12, I13, I23: TdReal); cdecl; external ODEDLL;
  procedure dMassAdd(var a,b: TdMass); cdecl; external ODEDLL;
  procedure dMassAdjust(var m: TdMass; newmass: TdReal); cdecl; external ODEDLL;
  procedure dMassTranslate(var m: TdMass; x, y, z: TdReal); cdecl; external ODEDLL;
  procedure dMassRotate(var m: TdMass; var R: TdMatrix3); cdecl; external ODEDLL;

  procedure dMassSetZero(var m: TdMass); cdecl; external ODEDLL;
  procedure dMassSetBox(var m: TdMass; density, lx, ly, lz: TdReal); cdecl; external ODEDLL;
  procedure dMassSetBoxTotal(var m: TdMass; total_mass, lx, ly, lz: TdReal); cdecl; external ODEDLL;
  procedure dMassSetCylinder(var m: TdMass; density: TdReal; direction: Integer; radius, length: TdReal); cdecl; external ODEDLL;
  procedure dMassSetCylinderTotal(var m: TdMass; total_mass: TdReal; direction: Integer; radius, length: TdReal); cdecl; external ODEDLL;
  procedure dMassSetCapsule(var m: TdMass; density: TdReal; direction: Integer; radius, length: TdReal); cdecl; external ODEDLL;
  procedure dMassSetCapsuleTotal(var m: TdMass; total_mass: TdReal; direction: Integer; radius, length: TdReal); cdecl; external ODEDLL;
  procedure dMassSetSphere(var m: TdMass; density, radius: TdReal); cdecl; external ODEDLL;
  procedure dMassSetSphereTotal(var m: TdMass; total_mass, radius: TdReal); cdecl; external ODEDLL;
  procedure dMassSetTrimesh(var m: TdMass; density: TdReal; trimesh:PdxGeom); cdecl; external ODEDLL;
  procedure dMassSetTrimeshTotal(var m: TdMass; total_mass: TdReal; trimesh:PdxGeom); cdecl; external ODEDLL;

  //----- Rotation.h -----
  procedure dQFromAxisAndAngle (var q: TdQuaternion; const ax, ay ,az, angle: TdReal); cdecl; external ODEDLL;
  procedure dRFromAxisAndAngle (var R: TdMatrix3; const ax, ay ,az, angle: TdReal); cdecl; external ODEDLL;
  procedure dRSetIdentity (var R: TdMatrix3); cdecl; external ODEDLL;
  procedure dQSetIdentity (var Q: TdQuaternion); cdecl; external ODEDLL;
  procedure dRFromEulerAngles (var R : TdMatrix3; const phi, theta, psi: TdReal); cdecl; external ODEDLL;
  procedure dRFrom2Axes (var R: TdMatrix3; const ax, ay, az, bx, by, bz: TdReal); cdecl; external ODEDLL;
  procedure dRFromZAxis (var R: TdMatrix3; const ax, ay, az: TdReal); cdecl; external ODEDLL;

  procedure dMultiply0 (const A: PdReal; const B, C: PdReal; p, q, r: integer); cdecl; external ODEDLL;
  procedure dMultiply1 (const A: PdReal; const B, C: PdReal; p, q, r: integer); cdecl; external ODEDLL;
  procedure dMultiply2 (const A: PdReal; const B, C: PdReal; p, q, r: integer); cdecl; external ODEDLL;
  procedure dQMultiply0 (var qa: TdQuaternion; const qb, qc: TdQuaternion); cdecl; external ODEDLL;
  procedure dQMultiply1 (var qa: TdQuaternion; const qb, qc: TdQuaternion); cdecl; external ODEDLL;
  procedure dQMultiply2 (var qa: TdQuaternion; const qb, qc: TdQuaternion); cdecl; external ODEDLL;
  procedure dQMultiply3 (var qa: TdQuaternion; const qb, qc: TdQuaternion); cdecl; external ODEDLL;
  procedure dRfromQ (var R: TdMatrix3; const q: TdQuaternion); cdecl; external ODEDLL;
  procedure dQfromR (var q: TdQuaternion; const R: TdMatrix3); cdecl; external ODEDLL;
  procedure dDQfromW (var dq: TdVector4; const w: TdVector3; const q: TdQuaternion); cdecl; external ODEDLL;

  //----- Math -----
  procedure dNormalize3 (var a: TdVector3); cdecl; external ODEDLL;
  procedure dNormalize4 (var a: TdVector4); cdecl; external ODEDLL;

  //----- Misc -----
  procedure dClosestLineSegmentPoints (const a1, a2, b1, b2: TdVector3; var cp1, cp2: TdVector3); cdecl; external ODEDLL;

  function dBoxTouchesBox (const _p1: TdVector3; const R1: TdMatrix3; const side1: TdVector3; const _p2: TdVector3; const R2: TdMatrix3; const side2: TdVector3): integer; cdecl; external ODEDLL;

  function dMaxDifference (A, B: PdReal; n, m: integer): TdReal; cdecl; external ODEDLL;
  procedure dMakeRandomVector(var n1: TdVector3; a: integer; f: TdReal); cdecl; external ODEDLL;
  function dAreConnected (a, b: PdxBody): integer; cdecl; external ODEDLL;
  function dAreConnectedExcluding (a, b: PdxBody; joint_type : TdJointTypeNumbers) : integer; cdecl; external ODEDLL;

  procedure dMakeRandomMatrix (A: PdRealArray; n, m: integer; range:  TdReal); cdecl; external ODEDLL;
  procedure dClearUpperTriangle (A: PdRealArray; n: integer); cdecl; external ODEDLL;

  function dRandGetSeed: Cardinal; cdecl; external ODEDLL;
  procedure dRandSetSeed (const s: Cardinal); cdecl; external ODEDLL;
  function dRandInt (const n: Integer): Integer; cdecl; external ODEDLL;
  function dRandReal: TdReal; cdecl; external ODEDLL;

  // return 1 if the random number generator is working.
  function dTestRand: Integer; cdecl; external ODEDLL;

  procedure dTestMatrixComparison; cdecl; external ODEDLL;
  procedure dTestSolveLCP; cdecl; external ODEDLL;

  //----- Recreated -----
  function dDot (const a, b: TdVector3): TdReal; overload;
  function dDot (const a, b: PdVector3): TdReal; overload;

  function dDOT14(const a,b: TdRealArray): TdReal; overload;
  function dDOT14(const a,b: PdRealArray): TdReal; overload;

  procedure dMULTIPLY0_333(var A: TdMatrix3; const B,C: TdMatrix3);
  procedure dMULTIPLY0_331(var A: TdVector3; const B: TdMatrix3; const C: TdVector3);

  function Vector3ScalarMul(const a: TdVector3; const Scalar : TdReal): TdVector3;
  function Vector3ADD(const a, b: TdVector3): TdVector3;
  function Vector3SUB(const a, b: TdVector3): TdVector3;
  function Vector3Length(const a : TdVector3): TdReal;
  function Vector3Cross(const V1, V2: TdVector3): TdVector3;
  function Vector3Make(const x,y,z: TdReal): TdVector3;

  procedure VerifyDelphiODE(Body: PdxBody; Geom: PdxGeom);

  {ExportInitODEMarker}

const MaxUserClasses = 4;

var
  dSphereClass : integer=0;
  dBoxClass : integer=1;
  dCapsuleClass : integer=2;
  dCylinderClass : integer=3;
  dPlaneClass : integer=4;
  dRayClass : integer=5;
  dConvexClass : integer=6;
  dGeomTransformClass : integer=7;
  dTriMeshClass : integer=8;
  dHeightFieldClass : integer=9;
  dFirstSpaceClass : integer = 10;
  dSimpleSpaceClass : integer = 10;
  dHashSpaceClass : integer = 11;
  dSweepAndPruneSpaceClass : integer = 12;
  dQuadTreeSpaceClass : integer = 13;
  dLastSpaceClass : integer = 13;
  dFirstUserClass : integer = 14;
  dLastUserClass : integer = 17;
  dGeomNumClasses : integer = 18;

  IsODEInitialized : boolean = False;
  DisabledDebugGeom : boolean = False;
  DisabledDebugCollision : boolean = False;

{$IFDEF cODEDebugEnabled}
var
   ODEDebugGeomList: TGeomList;
{$ENDIF}
{$IFDEF cODEDebugCollisionEnabled}
var
   ODEDebugCollisionList: array of TdContact;
{$ENDIF}


  // These are made public in the dynamic version MRQZZZ
  function InitODE(ADllName : PChar) : boolean;
  procedure CloseODE;


//-----------------------------------------
implementation
//-----------------------------------------

//---------------------
// TBodyList
//---------------------

procedure TBodyList.DeleteAllBodies;
var i : integer;
begin
  for i := 0 to Count-1 do  dBodyDestroy(Get(i));
  Clear;
end;

function TBodyList.GetItems(i: integer): PdxBody;
begin
  result := Get(i);
end;

procedure TBodyList.SetItems(i: integer; const Value: PdxBody);
begin
  Put(i, Value);
end;

//---------------------------
// TGeomList
//---------------------------

procedure TGeomList.DeleteAllGeoms(DeleteDataAsObject : boolean=false);
var i : integer;
    geom : PdxGeom;
begin
   for i := 0 to Count-1 do begin
      geom := Get(i);
      if DeleteDataAsObject and (geom.data<>nil) then TObject(geom.data).Free;
      dGeomDestroy(geom);
   end;
   Clear;
end;

function TGeomList.GetItems(i: integer): PdxGeom;
begin
   result := Get(i);
end;

procedure TGeomList.SetItems(i: integer; const Value: PdxGeom);
begin
   Put(i, Value);
end;

//----- Recreated -----

function dDot (const a, b : PdVector3) : TdReal;
begin
  Assert(Assigned(a),'a not assigned!');
  Assert(Assigned(b),'b not assigned!');
  result := ((a)[0]*(b)[0] + (a)[1]*(b)[1] + (a)[2]*(b)[2]);
end;

function dDot (const a, b : TdVector3) : TdReal;
begin
  result := ((a)[0]*(b)[0] + (a)[1]*(b)[1] + (a)[2]*(b)[2]);
end;

// #define dDOT(a,b)   ((a)[0]*(b)[0] + (a)[1]*(b)[1] + (a)[2]*(b)[2])
// #define dDOT14(a,b) ((a)[0]*(b)[0] + (a)[1]*(b)[4] + (a)[2]*(b)[8])
// #define dDOT41(a,b) ((a)[0]*(b)[0] + (a)[4]*(b)[1] + (a)[8]*(b)[2])
// #define dDOT44(a,b) ((a)[0]*(b)[0] + (a)[4]*(b)[4] + (a)[8]*(b)[8])

function dDOT14(const a,b : TdRealArray) : TdReal; overload;
begin
  result := ((a)[0]*(b)[0] + (a)[1]*(b)[4] + (a)[2]*(b)[8]);
end;

function dDOT14(const a,b : PdRealArray) : TdReal; overload;
begin
  result := ((a)[0]*(b)[0] + (a)[1]*(b)[4] + (a)[2]*(b)[8]);
end;

procedure dMULTIPLY0_331(var A : TdVector3; const B : TdMatrix3; const C : TdVector3);
{var
  v : PdVector3;}
begin
  // #define dMULTIPLY0_331(A,B,C) dMULTIPLYOP0_331(A,=,B,C)

  //  #define dMULTIPLYOP0_331(A,op,B,C) \
  //    (A)[0] op dDOT((B),(C)); \
  //    (A)[1] op dDOT((B+4),(C)); \
  //    (A)[2] op dDOT((B+8),(C));


  A[0] := dDOT(PdVector3(@(B[0]))^, C);

  A[1] := dDOT(PdVector3(@(B[4]))^, C);
  A[2] := dDOT(PdVector3(@(B[8]))^, C);//}
end;

procedure dMULTIPLY0_333(var A : TdMatrix3; const B,C : TdMatrix3);
begin
  // #define dMULTIPLY0_333(A,B,C) dMULTIPLYOP0_333(A,=,B,C)
  // #define dMULTIPLYOP0_333(A,op,B,C) \
  //   (A)[0] op dDOT14((B),(C)); \
  //   (A)[1] op dDOT14((B),(C+1)); \
  //   (A)[2] op dDOT14((B),(C+2)); \
  //   (A)[4] op dDOT14((B+4),(C)); \
  //   (A)[5] op dDOT14((B+4),(C+1)); \
  //   (A)[6] op dDOT14((B+4),(C+2)); \
  //   (A)[8] op dDOT14((B+8),(C)); \
  //   (A)[9] op dDOT14((B+8),(C+1)); \
  //   (A)[10] op dDOT14((B+8),(C+2));

  A[0] := dDOT14(PdRealArray(@(B[0])),PdRealArray(@(C[0])));
  A[1] := dDOT14(PdRealArray(@(B[0])),PdRealArray(@(C[1])));
  A[2] := dDOT14(PdRealArray(@(B[0])),PdRealArray(@(C[2])));

  A[4] := dDOT14(PdRealArray(@(B[4])),PdRealArray(@(C[0])));
  A[5] := dDOT14(PdRealArray(@(B[4])),PdRealArray(@(C[1])));
  A[6] := dDOT14(PdRealArray(@(B[4])),PdRealArray(@(C[2])));

  A[8] := dDOT14(PdRealArray(@(B[8])),PdRealArray(@(C[0])));
  A[9] := dDOT14(PdRealArray(@(B[8])),PdRealArray(@(C[1])));
  A[10] := dDOT14(PdRealArray(@(B[8])),PdRealArray(@(C[2])));
end;

function Vector3ScalarMul(const a : TdVector3; const Scalar : TdReal) : TdVector3;
begin
  result[0] := a[0]*Scalar;
  result[1] := a[1]*Scalar;
  result[2] := a[2]*Scalar;
end;

function Vector3ADD(const a, b : TdVector3) : TdVector3;
begin
  result[0] := a[0]+b[0];
  result[1] := a[1]+b[1];
  result[2] := a[2]+b[2];
end;

function Vector3SUB(const a, b : TdVector3) : TdVector3;
begin
  result[0] := a[0]-b[0];
  result[1] := a[1]-b[1];
  result[2] := a[2]-b[2];
end;

function Vector3Length(const a : TdVector3) : TdReal;
begin
  result := sqrt(sqr(a[0])+sqr(a[1])+sqr(a[2]));
end;

function Vector3Cross(const V1, V2 : TdVector3) : TdVector3;
begin
   Result[0]:=V1[1] * V2[2] - V1[2] * V2[1];
   Result[1]:=V1[2] * V2[0] - V1[0] * V2[2];
   Result[2]:=V1[0] * V2[1] - V1[1] * V2[0];
end;

function Vector3Make(const x,y,z : TdReal) : TdVector3;
begin
  result[0] := x;
  result[1] := y;
  result[2] := z;
end;

(*
procedure DisableStillBodies(World : PdxWorld; Threshold : TdReal=0.0001);
var
  Body : PdxBody;
  TempList : TList;
begin
  if not Assigned(WasStillBefore) then
  begin
    WasStillBefore := TList.Create;
    WasStillBeforeOld := TList.Create;
  end;

  Body := World.FirstBody;

  WasStillBefore.Clear;

  // We can't disable bodies just as soon as they're still - that could disable
  // bodies that are just slowed down or titering on an edge. If they've been
  // still for two frames, we consider them truly still.
  while Assigned(Body) do
  begin
    if dBodyIsEnabled(Body)=1 then
    begin
      // Is the body still?
      if (abs(Body.lvel[0])<Threshold) and (abs(Body.lvel[1])<Threshold) and (abs(Body.lvel[2])<Threshold) and
         (abs(Body.avel[0])<Threshold) and (abs(Body.avel[1])<Threshold) and (abs(Body.avel[2])<Threshold) then
      begin
        if WasStillBeforeOld.IndexOf(Body)<>-1 then
          dBodyDisable(Body)
        else
          WasStillBefore.Add(Body);
      end;
    end;

    Body := PdxBody(Body.BaseObject.next);
  end;

  TempList := WasStillBeforeOld;
  WasStillBeforeOld := WasStillBefore;
  WasStillBefore := TempList;
end; *)

procedure VerifyDelphiODE(Body : PdxBody; Geom : PdxGeom);
var
  m : TdMass;
  VerificationPointer : pointer;
begin
  VerificationPointer := pointer( -1 ); // A known pointer
  // Verify Body
  dBodySetData( Body, VerificationPointer );
  Assert( dBodyGetData( Body ) = VerificationPointer, 'Body test 1 fails' );
  Assert( Body.BaseObject.userdata = VerificationPointer, 'Body test 2 fails' );

  dBodyGetMass(Body, m);

  Assert(Body.mass.mass = m.mass, 'Body test 3 fails');

  // Verify Geom
  dGeomSetData( Geom, VerificationPointer );
  Assert( dGeomGetData( Geom ) = VerificationPointer, 'Geom test 1 fails' );
  Assert(dGeomGetBody(Geom)=Geom.Body, 'Geom test 2 fails');
  Assert( Geom.Data = VerificationPointer, 'Geom test 3 fails' );
end;

var
  vODEHandle : TModuleHandle;

procedure GetODEClassIDs;
begin
  {$IFDEF PARODE}
   dSphereClass:=dSphereGetClass;
   dBoxClass:=dBoxGetClass;
   dPlaneClass:=dPlaneGetClass;
   dCylinderClass:=dCylinderGetClass;
   dConvexClass:=dConvexGetClass;
   dGeomTransformClass:=dGeomTransformGetClass;
   dRayClass:=dRayGetClass;
   dTriMeshClass:=dTriMeshGetClass;
   dHeightfieldClass:=dHeightfieldGetClass;
   {$ENDIF}
end;

function InitODE(ADllName : PChar) : boolean;
var isODELoaded : boolean;
begin
  result := IsODEInitialized;
  if IsODEInitialized then exit;

  if ADllName = '' then ADllName := ODEDLL;

  isODELoaded := LoadModule( vODEHandle, ADllName );
  if not isODELoaded then exit;

  if isODELoaded and not IsODEInitialized then begin
     dInitODE2(0);
     GetODEClassIDs;
     IsODEInitialized:=True;
  end;
  result:=IsODEInitialized;
end;

procedure CloseODE;
begin
  if IsODEInitialized then dCloseODE;
  IsODEInitialized := false;
  UnLoadModule( vODEHandle );
end;


procedure jm_print_dll_sizes();
begin
  WriteLn();
  WriteLn();
  WriteLn('TdxJointGroup');
  WriteLn('num: ', PtrUInt(@TdxJointGroup(Nil^).num),'  : ',sizeOf(TdxJointGroup.num));
  WriteLn('stack: ', PtrUInt(@TdxJointGroup(Nil^).stack),'  : ',sizeOf(TdxJointGroup.stack));
  WriteLn('sizeof: ', sizeOf(TdxJointGroup));
  WriteLn();
  WriteLn();
  WriteLn('TdxJointLimitMotor');
  WriteLn('vel: ', PtrUInt(@TdxJointLimitMotor(Nil^).vel),'  : ',sizeOf(TdxJointLimitMotor.vel));
  WriteLn('fmax: ', PtrUInt(@TdxJointLimitMotor(Nil^).fmax),'  : ',sizeOf(TdxJointLimitMotor.fmax));
  WriteLn('lostop: ', PtrUInt(@TdxJointLimitMotor(Nil^).lostop),'  : ',sizeOf(TdxJointLimitMotor.lostop));
  WriteLn('histop: ', PtrUInt(@TdxJointLimitMotor(Nil^).histop),'  : ',sizeOf(TdxJointLimitMotor.histop));
  WriteLn('fudge_factor: ', PtrUInt(@TdxJointLimitMotor(Nil^).fudge_factor),'  : ',sizeOf(TdxJointLimitMotor.fudge_factor));
  WriteLn('normal_cfm: ', PtrUInt(@TdxJointLimitMotor(Nil^).normal_cfm),'  : ',sizeOf(TdxJointLimitMotor.normal_cfm));
  WriteLn('stop_erp: ', PtrUInt(@TdxJointLimitMotor(Nil^).stop_erp),'  : ',sizeOf(TdxJointLimitMotor.stop_erp));
  WriteLn('stop_cfm: ', PtrUInt(@TdxJointLimitMotor(Nil^).stop_cfm),'  : ',sizeOf(TdxJointLimitMotor.stop_cfm));
  WriteLn('bounce: ', PtrUInt(@TdxJointLimitMotor(Nil^).bounce),'  : ',sizeOf(TdxJointLimitMotor.bounce));
  WriteLn('limit: ', PtrUInt(@TdxJointLimitMotor(Nil^).limit),'  : ',sizeOf(TdxJointLimitMotor.limit));
  WriteLn('limit_err: ', PtrUInt(@TdxJointLimitMotor(Nil^).limit_err),'  : ',sizeOf(TdxJointLimitMotor.limit_err));
  WriteLn('sizeof: ', sizeOf(TdxJointLimitMotor));
  WriteLn();
  WriteLn();
  WriteLn('TdMass');
  WriteLn('mass: ', PtrUInt(@TdMass(Nil^).mass),'  : ',sizeOf(TdMass.mass));
  WriteLn('c: ', PtrUInt(@TdMass(Nil^).c),'  : ',sizeOf(TdMass.c));
  WriteLn('I: ', PtrUInt(@TdMass(Nil^).I),'  : ',sizeOf(TdMass.I));
  WriteLn('sizeof: ', sizeOf(TdMass));
  WriteLn();
  WriteLn();
  WriteLn('TdxAutoDisable');
  WriteLn('idle_time: ', PtrUInt(@TdxAutoDisable(Nil^).idle_time),'  : ',sizeOf(TdxAutoDisable.idle_time));
  WriteLn('idle_steps: ', PtrUInt(@TdxAutoDisable(Nil^).idle_steps),'  : ',sizeOf(TdxAutoDisable.idle_steps));
  WriteLn('linear_average_threashold: ', PtrUInt(@TdxAutoDisable(Nil^).linear_average_threashold),'  : ',sizeOf(TdxAutoDisable.linear_average_threashold));
  WriteLn('angular_average_threashold: ', PtrUInt(@TdxAutoDisable(Nil^).angular_average_threashold),'  : ',sizeOf(TdxAutoDisable.angular_average_threashold));
  WriteLn('average_samples: ', PtrUInt(@TdxAutoDisable(Nil^).average_samples),'  : ',sizeOf(TdxAutoDisable.average_samples));
  WriteLn('sizeof: ', sizeOf(TdxAutoDisable));
  WriteLn();
  WriteLn();
  WriteLn('TdxDampingParameters');
  WriteLn('linear_scale: ', PtrUInt(@TdxDampingParameters(Nil^).linear_scale),'  : ',sizeOf(TdxDampingParameters.linear_scale));
  WriteLn('angular_scale: ', PtrUInt(@TdxDampingParameters(Nil^).angular_scale),'  : ',sizeOf(TdxDampingParameters.angular_scale));
  WriteLn('linear_threahold: ', PtrUInt(@TdxDampingParameters(Nil^).linear_threahold),'  : ',sizeOf(TdxDampingParameters.linear_threahold));
  WriteLn('angular_threashold: ', PtrUInt(@TdxDampingParameters(Nil^).angular_threashold),'  : ',sizeOf(TdxDampingParameters.angular_threashold));
  WriteLn('sizeof: ', sizeOf(TdxDampingParameters));
  WriteLn();
  WriteLn();
  WriteLn('TdxContactParameters');
  WriteLn('max_vel: ', PtrUInt(@TdxContactParameters(Nil^).max_vel),'  : ',sizeOf(TdxContactParameters.max_vel));
  WriteLn('min_depth: ', PtrUInt(@TdxContactParameters(Nil^).min_depth),'  : ',sizeOf(TdxContactParameters.min_depth));
  WriteLn('sizeof: ', sizeOf(TdxContactParameters));
  WriteLn();
  WriteLn();
  WriteLn('TdxQuickStepParameters');
  WriteLn('num_iterations: ', PtrUInt(@TdxQuickStepParameters(Nil^).num_iterations),'  : ',sizeOf(TdxQuickStepParameters.num_iterations));
  WriteLn('w: ', PtrUInt(@TdxQuickStepParameters(Nil^).w),'  : ',sizeOf(TdxQuickStepParameters.w));
  WriteLn('sizeof: ', sizeOf(TdxQuickStepParameters));
  WriteLn();
  WriteLn();
  WriteLn('TdObject');
  WriteLn('World: ', PtrUInt(@TdObject(Nil^).World),'  : ',sizeOf(TdObject.World));
  WriteLn('next: ', PtrUInt(@TdObject(Nil^).next),'  : ',sizeOf(TdObject.next));
  WriteLn('tome: ', PtrUInt(@TdObject(Nil^).tome),'  : ',sizeOf(TdObject.tome));
  WriteLn('userdata: ', PtrUInt(@TdObject(Nil^).userdata),'  : ',sizeOf(TdObject.userdata));
  WriteLn('tag: ', PtrUInt(@TdObject(Nil^).tag),'  : ',sizeOf(TdObject.tag));
  WriteLn('sizeof: ', sizeOf(TdObject));
  WriteLn();
  WriteLn();
  WriteLn('TdxJointNode');
  WriteLn('joint: ', PtrUInt(@TdxJointNode(Nil^).joint),'  : ',sizeOf(TdxJointNode.joint));
  WriteLn('body: ', PtrUInt(@TdxJointNode(Nil^).body),'  : ',sizeOf(TdxJointNode.body));
  WriteLn('next: ', PtrUInt(@TdxJointNode(Nil^).next),'  : ',sizeOf(TdxJointNode.next));
  WriteLn('sizeof: ', sizeOf(TdxJointNode));
  WriteLn();
  WriteLn();
  WriteLn('TJointInfo1');
  WriteLn('m: ', PtrUInt(@TJointInfo1(Nil^).m),'  : ',sizeOf(TJointInfo1.m));
  WriteLn('nub: ', PtrUInt(@TJointInfo1(Nil^).nub),'  : ',sizeOf(TJointInfo1.nub));
  WriteLn('sizeof: ', sizeOf(TJointInfo1));
  WriteLn();
  WriteLn();
  WriteLn('TJointSureMaxInfo');
  WriteLn('max_m: ', PtrUInt(@TJointSureMaxInfo(Nil^).max_m),'  : ',sizeOf(TJointSureMaxInfo.max_m));
  WriteLn('sizeof: ', sizeOf(TJointSureMaxInfo));
  WriteLn();
  WriteLn();
  {WriteLn('TJointInfo2');
  WriteLn('fps: ', PtrUInt(@TJointInfo2(Nil^).fps),'  : ',sizeOf(TJointInfo2.fps));
  WriteLn('erp: ', PtrUInt(@TJointInfo2(Nil^).erp),'  : ',sizeOf(TJointInfo2.erp));
  WriteLn('J1l: ', PtrUInt(@TJointInfo2(Nil^).J1l),'  : ',sizeOf(TJointInfo2.J1l));
  WriteLn('J1a: ', PtrUInt(@TJointInfo2(Nil^).J1a),'  : ',sizeOf(TJointInfo2.J1a));
  WriteLn('J2l: ', PtrUInt(@TJointInfo2(Nil^).J2l),'  : ',sizeOf(TJointInfo2.J2l));
  WriteLn('J2a: ', PtrUInt(@TJointInfo2(Nil^).J2a),'  : ',sizeOf(TJointInfo2.J2a));
  WriteLn('rowskip: ', PtrUInt(@TJointInfo2(Nil^).rowskip),'  : ',sizeOf(TJointInfo2.rowskip));
  WriteLn('c: ', PtrUInt(@TJointInfo2(Nil^).c),'  : ',sizeOf(TJointInfo2.c));
  WriteLn('cfm: ', PtrUInt(@TJointInfo2(Nil^).cfm),'  : ',sizeOf(TJointInfo2.cfm));
  WriteLn('lo: ', PtrUInt(@TJointInfo2(Nil^).lo),'  : ',sizeOf(TJointInfo2.lo));
  WriteLn('hi: ', PtrUInt(@TJointInfo2(Nil^).hi),'  : ',sizeOf(TJointInfo2.hi));
  WriteLn('findex: ', PtrUInt(@TJointInfo2(Nil^).findex),'  : ',sizeOf(TJointInfo2.findex));
  WriteLn('sizeof: ', sizeOf(TJointInfo2));
  WriteLn();
  WriteLn();}
  WriteLn('TdxJoint');
  WriteLn('baseObject: ', PtrUInt(@TdxJoint(Nil^).baseObject),'  : ',sizeOf(TdxJoint.baseObject));
  //WriteLn('Info1: ', PtrUInt(@TdxJoint(Nil^).Info1),'  : ',sizeOf(TdxJoint.Info1));
  //WriteLn('SureMaxInfo: ', PtrUInt(@TdxJoint(Nil^).SureMaxInfo),'  : ',sizeOf(TdxJoint.SureMaxInfo));
  WriteLn('flags: ', PtrUInt(@TdxJoint(Nil^).flags),'  : ',sizeOf(TdxJoint.flags));
  WriteLn('node: ', PtrUInt(@TdxJoint(Nil^).node),'  : ',sizeOf(TdxJoint.node));
  WriteLn('feedback: ', PtrUInt(@TdxJoint(Nil^).feedback),'  : ',sizeOf(TdxJoint.feedback));
  WriteLn('lambda: ', PtrUInt(@TdxJoint(Nil^).lambda),'  : ',sizeOf(TdxJoint.lambda));
  WriteLn('sizeof: ', sizeOf(TdxJoint));
  WriteLn();
  WriteLn();
  WriteLn('TdxJointBall');
  WriteLn('BaseJoint: ', PtrUInt(@TdxJointBall(Nil^).BaseJoint),'  : ',sizeOf(TdxJointBall.BaseJoint));
  WriteLn('anchor1: ', PtrUInt(@TdxJointBall(Nil^).anchor1),'  : ',sizeOf(TdxJointBall.anchor1));
  WriteLn('anchor2: ', PtrUInt(@TdxJointBall(Nil^).anchor2),'  : ',sizeOf(TdxJointBall.anchor2));
  WriteLn('erp: ', PtrUInt(@TdxJointBall(Nil^).erp),'  : ',sizeOf(TdxJointBall.erp));
  WriteLn('cfm: ', PtrUInt(@TdxJointBall(Nil^).cfm),'  : ',sizeOf(TdxJointBall.cfm));
  WriteLn('sizeof: ', sizeOf(TdxJointBall));
  WriteLn();
  WriteLn();
  WriteLn('TdxJointHinge');
  WriteLn('BaseJoint: ', PtrUInt(@TdxJointHinge(Nil^).BaseJoint),'  : ',sizeOf(TdxJointHinge.BaseJoint));
  WriteLn('anchor1: ', PtrUInt(@TdxJointHinge(Nil^).anchor1),'  : ',sizeOf(TdxJointHinge.anchor1));
  WriteLn('anchor2: ', PtrUInt(@TdxJointHinge(Nil^).anchor2),'  : ',sizeOf(TdxJointHinge.anchor2));
  WriteLn('axis1: ', PtrUInt(@TdxJointHinge(Nil^).axis1),'  : ',sizeOf(TdxJointHinge.axis1));
  WriteLn('axis2: ', PtrUInt(@TdxJointHinge(Nil^).axis2),'  : ',sizeOf(TdxJointHinge.axis2));
  WriteLn('qrel: ', PtrUInt(@TdxJointHinge(Nil^).qrel),'  : ',sizeOf(TdxJointHinge.qrel));
  WriteLn('limot: ', PtrUInt(@TdxJointHinge(Nil^).limot),'  : ',sizeOf(TdxJointHinge.limot));
  WriteLn('sizeof: ', sizeOf(TdxJointHinge));
  WriteLn();
  WriteLn();
  WriteLn('TdxJointUniversial');
  WriteLn('BaseJoint: ', PtrUInt(@TdxJointUniversial(Nil^).BaseJoint),'  : ',sizeOf(TdxJointUniversial.BaseJoint));
  WriteLn('anchor1: ', PtrUInt(@TdxJointUniversial(Nil^).anchor1),'  : ',sizeOf(TdxJointUniversial.anchor1));
  WriteLn('anchor2: ', PtrUInt(@TdxJointUniversial(Nil^).anchor2),'  : ',sizeOf(TdxJointUniversial.anchor2));
  WriteLn('axis1: ', PtrUInt(@TdxJointUniversial(Nil^).axis1),'  : ',sizeOf(TdxJointUniversial.axis1));
  WriteLn('axis2: ', PtrUInt(@TdxJointUniversial(Nil^).axis2),'  : ',sizeOf(TdxJointUniversial.axis2));
  WriteLn('qrel1: ', PtrUInt(@TdxJointUniversial(Nil^).qrel1),'  : ',sizeOf(TdxJointUniversial.qrel1));
  WriteLn('qrel2: ', PtrUInt(@TdxJointUniversial(Nil^).qrel2),'  : ',sizeOf(TdxJointUniversial.qrel2));
  WriteLn('limot1: ', PtrUInt(@TdxJointUniversial(Nil^).limot1),'  : ',sizeOf(TdxJointUniversial.limot1));
  WriteLn('limot2: ', PtrUInt(@TdxJointUniversial(Nil^).limot2),'  : ',sizeOf(TdxJointUniversial.limot2));
  WriteLn('sizeof: ', sizeOf(TdxJointUniversial));
  WriteLn();
  WriteLn();
  WriteLn('TdxJointPR');
  WriteLn('BaseJoint: ', PtrUInt(@TdxJointPR(Nil^).BaseJoint),'  : ',sizeOf(TdxJointPR.BaseJoint));
  WriteLn('anchor2: ', PtrUInt(@TdxJointPR(Nil^).anchor2),'  : ',sizeOf(TdxJointPR.anchor2));
  WriteLn('axisR1: ', PtrUInt(@TdxJointPR(Nil^).axisR1),'  : ',sizeOf(TdxJointPR.axisR1));
  WriteLn('axisR2: ', PtrUInt(@TdxJointPR(Nil^).axisR2),'  : ',sizeOf(TdxJointPR.axisR2));
  WriteLn('axisP1: ', PtrUInt(@TdxJointPR(Nil^).axisP1),'  : ',sizeOf(TdxJointPR.axisP1));
  WriteLn('qrel: ', PtrUInt(@TdxJointPR(Nil^).qrel),'  : ',sizeOf(TdxJointPR.qrel));
  WriteLn('offset: ', PtrUInt(@TdxJointPR(Nil^).offset),'  : ',sizeOf(TdxJointPR.offset));
  WriteLn('limotR: ', PtrUInt(@TdxJointPR(Nil^).limotR),'  : ',sizeOf(TdxJointPR.limotR));
  WriteLn('limotP: ', PtrUInt(@TdxJointPR(Nil^).limotP),'  : ',sizeOf(TdxJointPR.limotP));
  WriteLn('sizeof: ', sizeOf(TdxJointPR));
  WriteLn();
  WriteLn();
  WriteLn('TdxJointPiston');
  WriteLn('BaseJoint: ', PtrUInt(@TdxJointPiston(Nil^).BaseJoint),'  : ',sizeOf(TdxJointPiston.BaseJoint));
  WriteLn('axis1: ', PtrUInt(@TdxJointPiston(Nil^).axis1),'  : ',sizeOf(TdxJointPiston.axis1));
  WriteLn('axis2: ', PtrUInt(@TdxJointPiston(Nil^).axis2),'  : ',sizeOf(TdxJointPiston.axis2));
  WriteLn('qrel: ', PtrUInt(@TdxJointPiston(Nil^).qrel),'  : ',sizeOf(TdxJointPiston.qrel));
  WriteLn('anchor1: ', PtrUInt(@TdxJointPiston(Nil^).anchor1),'  : ',sizeOf(TdxJointPiston.anchor1));
  WriteLn('anchor2: ', PtrUInt(@TdxJointPiston(Nil^).anchor2),'  : ',sizeOf(TdxJointPiston.anchor2));
  WriteLn('limotP: ', PtrUInt(@TdxJointPiston(Nil^).limotP),'  : ',sizeOf(TdxJointPiston.limotP));
  WriteLn('limotR: ', PtrUInt(@TdxJointPiston(Nil^).limotR),'  : ',sizeOf(TdxJointPiston.limotR));
  WriteLn('sizeof: ', sizeOf(TdxJointPiston));
  WriteLn();
  WriteLn();
  WriteLn('TdxJointSlider');
  WriteLn('BaseJoint: ', PtrUInt(@TdxJointSlider(Nil^).BaseJoint),'  : ',sizeOf(TdxJointSlider.BaseJoint));
  WriteLn('axis1: ', PtrUInt(@TdxJointSlider(Nil^).axis1),'  : ',sizeOf(TdxJointSlider.axis1));
  WriteLn('qrel: ', PtrUInt(@TdxJointSlider(Nil^).qrel),'  : ',sizeOf(TdxJointSlider.qrel));
  WriteLn('offset: ', PtrUInt(@TdxJointSlider(Nil^).offset),'  : ',sizeOf(TdxJointSlider.offset));
  WriteLn('limot: ', PtrUInt(@TdxJointSlider(Nil^).limot),'  : ',sizeOf(TdxJointSlider.limot));
  WriteLn('sizeof: ', sizeOf(TdxJointSlider));
  WriteLn();
  WriteLn();
  WriteLn('TdxJointHinge2');
  WriteLn('BaseJoint: ', PtrUInt(@TdxJointHinge2(Nil^).BaseJoint),'  : ',sizeOf(TdxJointHinge2.BaseJoint));
  WriteLn('anchor1: ', PtrUInt(@TdxJointHinge2(Nil^).anchor1),'  : ',sizeOf(TdxJointHinge2.anchor1));
  WriteLn('anchor2: ', PtrUInt(@TdxJointHinge2(Nil^).anchor2),'  : ',sizeOf(TdxJointHinge2.anchor2));
  WriteLn('axis1: ', PtrUInt(@TdxJointHinge2(Nil^).axis1),'  : ',sizeOf(TdxJointHinge2.axis1));
  WriteLn('axis2: ', PtrUInt(@TdxJointHinge2(Nil^).axis2),'  : ',sizeOf(TdxJointHinge2.axis2));
  WriteLn('c0: ', PtrUInt(@TdxJointHinge2(Nil^).c0),'  : ',sizeOf(TdxJointHinge2.c0));
  WriteLn('s0: ', PtrUInt(@TdxJointHinge2(Nil^).s0),'  : ',sizeOf(TdxJointHinge2.s0));
  WriteLn('v1: ', PtrUInt(@TdxJointHinge2(Nil^).v1),'  : ',sizeOf(TdxJointHinge2.v1));
  WriteLn('v2: ', PtrUInt(@TdxJointHinge2(Nil^).v2),'  : ',sizeOf(TdxJointHinge2.v2));
  WriteLn('limot1: ', PtrUInt(@TdxJointHinge2(Nil^).limot1),'  : ',sizeOf(TdxJointHinge2.limot1));
  WriteLn('limot2: ', PtrUInt(@TdxJointHinge2(Nil^).limot2),'  : ',sizeOf(TdxJointHinge2.limot2));
  WriteLn('susp_erp: ', PtrUInt(@TdxJointHinge2(Nil^).susp_erp),'  : ',sizeOf(TdxJointHinge2.susp_erp));
  WriteLn('susp_cfm: ', PtrUInt(@TdxJointHinge2(Nil^).susp_cfm),'  : ',sizeOf(TdxJointHinge2.susp_cfm));
  WriteLn('sizeof: ', sizeOf(TdxJointHinge2));
  WriteLn();
  WriteLn();
  WriteLn('TdxJointAMotor');
  WriteLn('BaseJoint: ', PtrUInt(@TdxJointAMotor(Nil^).BaseJoint),'  : ',sizeOf(TdxJointAMotor.BaseJoint));
  WriteLn('mode: ', PtrUInt(@TdxJointAMotor(Nil^).mode),'  : ',sizeOf(TdxJointAMotor.mode));
  WriteLn('num: ', PtrUInt(@TdxJointAMotor(Nil^).num),'  : ',sizeOf(TdxJointAMotor.num));
  WriteLn('rel: ', PtrUInt(@TdxJointAMotor(Nil^).rel),'  : ',sizeOf(TdxJointAMotor.rel));
  WriteLn('axis: ', PtrUInt(@TdxJointAMotor(Nil^).axis),'  : ',sizeOf(TdxJointAMotor.axis));
  WriteLn('references: ', PtrUInt(@TdxJointAMotor(Nil^).references),'  : ',sizeOf(TdxJointAMotor.references));
  WriteLn('angle: ', PtrUInt(@TdxJointAMotor(Nil^).angle),'  : ',sizeOf(TdxJointAMotor.angle));
  WriteLn('limot: ', PtrUInt(@TdxJointAMotor(Nil^).limot),'  : ',sizeOf(TdxJointAMotor.limot));
  WriteLn('sizeof: ', sizeOf(TdxJointAMotor));
  WriteLn();
  WriteLn();
  WriteLn('TdxJointLMotor');
  WriteLn('BaseJoint: ', PtrUInt(@TdxJointLMotor(Nil^).BaseJoint),'  : ',sizeOf(TdxJointLMotor.BaseJoint));
  WriteLn('num: ', PtrUInt(@TdxJointLMotor(Nil^).num),'  : ',sizeOf(TdxJointLMotor.num));
  WriteLn('rel: ', PtrUInt(@TdxJointLMotor(Nil^).rel),'  : ',sizeOf(TdxJointLMotor.rel));
  WriteLn('axis: ', PtrUInt(@TdxJointLMotor(Nil^).axis),'  : ',sizeOf(TdxJointLMotor.axis));
  WriteLn('limot: ', PtrUInt(@TdxJointLMotor(Nil^).limot),'  : ',sizeOf(TdxJointLMotor.limot));
  WriteLn('sizeof: ', sizeOf(TdxJointLMotor));
  WriteLn();
  WriteLn();
  WriteLn('TdxJointPlane2D');
  WriteLn('BaseJoint: ', PtrUInt(@TdxJointPlane2D(Nil^).BaseJoint),'  : ',sizeOf(TdxJointPlane2D.BaseJoint));
  WriteLn('row_motor_x: ', PtrUInt(@TdxJointPlane2D(Nil^).row_motor_x),'  : ',sizeOf(TdxJointPlane2D.row_motor_x));
  WriteLn('row_motor_y: ', PtrUInt(@TdxJointPlane2D(Nil^).row_motor_y),'  : ',sizeOf(TdxJointPlane2D.row_motor_y));
  WriteLn('row_motor_angle: ', PtrUInt(@TdxJointPlane2D(Nil^).row_motor_angle),'  : ',sizeOf(TdxJointPlane2D.row_motor_angle));
  WriteLn('motor_x: ', PtrUInt(@TdxJointPlane2D(Nil^).motor_x),'  : ',sizeOf(TdxJointPlane2D.motor_x));
  WriteLn('motor_y: ', PtrUInt(@TdxJointPlane2D(Nil^).motor_y),'  : ',sizeOf(TdxJointPlane2D.motor_y));
  WriteLn('motor_angle: ', PtrUInt(@TdxJointPlane2D(Nil^).motor_angle),'  : ',sizeOf(TdxJointPlane2D.motor_angle));
  WriteLn('sizeof: ', sizeOf(TdxJointPlane2D));
  WriteLn();
  WriteLn();
  WriteLn('TdxJointFixed');
  WriteLn('BaseJoint: ', PtrUInt(@TdxJointFixed(Nil^).BaseJoint),'  : ',sizeOf(TdxJointFixed.BaseJoint));
  WriteLn('qrel: ', PtrUInt(@TdxJointFixed(Nil^).qrel),'  : ',sizeOf(TdxJointFixed.qrel));
  WriteLn('offset: ', PtrUInt(@TdxJointFixed(Nil^).offset),'  : ',sizeOf(TdxJointFixed.offset));
  WriteLn('erp: ', PtrUInt(@TdxJointFixed(Nil^).erp),'  : ',sizeOf(TdxJointFixed.erp));
  WriteLn('cfm: ', PtrUInt(@TdxJointFixed(Nil^).cfm),'  : ',sizeOf(TdxJointFixed.cfm));
  WriteLn('sizeof: ', sizeOf(TdxJointFixed));
  WriteLn();
  WriteLn();
  WriteLn('TdxPosR');
  WriteLn('pos: ', PtrUInt(@TdxPosR(Nil^).pos),'  : ',sizeOf(TdxPosR.pos));
  WriteLn('R: ', PtrUInt(@TdxPosR(Nil^).R),'  : ',sizeOf(TdxPosR.R));
  WriteLn('sizeof: ', sizeOf(TdxPosR));
  WriteLn();
  WriteLn();
  WriteLn('TdxBody');
  WriteLn('BaseObject: ', PtrUInt(@TdxBody(Nil^).BaseObject),'  : ',sizeOf(TdxBody.BaseObject));
  //WriteLn('Padding: ', PtrUInt(@TdxBody(Nil^).Padding),'  : ',sizeOf(TdxBody.Padding));
  WriteLn('firstjoint: ', PtrUInt(@TdxBody(Nil^).firstjoint),'  : ',sizeOf(TdxBody.firstjoint));
  WriteLn('flags: ', PtrUInt(@TdxBody(Nil^).flags),'  : ',sizeOf(TdxBody.flags));
  WriteLn('geom: ', PtrUInt(@TdxBody(Nil^).geom),'  : ',sizeOf(TdxBody.geom));
  WriteLn('mass: ', PtrUInt(@TdxBody(Nil^).mass),'  : ',sizeOf(TdxBody.mass));
  WriteLn('invI: ', PtrUInt(@TdxBody(Nil^).invI),'  : ',sizeOf(TdxBody.invI));
  WriteLn('invMass: ', PtrUInt(@TdxBody(Nil^).invMass),'  : ',sizeOf(TdxBody.invMass));
  WriteLn('posr: ', PtrUInt(@TdxBody(Nil^).posr),'  : ',sizeOf(TdxBody.posr));
  WriteLn('q: ', PtrUInt(@TdxBody(Nil^).q),'  : ',sizeOf(TdxBody.q));
  WriteLn('lvel: ', PtrUInt(@TdxBody(Nil^).lvel),'  : ',sizeOf(TdxBody.lvel));
  WriteLn('avel: ', PtrUInt(@TdxBody(Nil^).avel),'  : ',sizeOf(TdxBody.avel));
  WriteLn('facc: ', PtrUInt(@TdxBody(Nil^).facc),'  : ',sizeOf(TdxBody.facc));
  WriteLn('tacc: ', PtrUInt(@TdxBody(Nil^).tacc),'  : ',sizeOf(TdxBody.tacc));
  WriteLn('finite_rot_axis: ', PtrUInt(@TdxBody(Nil^).finite_rot_axis),'  : ',sizeOf(TdxBody.finite_rot_axis));
  WriteLn('adis: ', PtrUInt(@TdxBody(Nil^).adis),'  : ',sizeOf(TdxBody.adis));
  WriteLn('adis_timeleft: ', PtrUInt(@TdxBody(Nil^).adis_timeleft),'  : ',sizeOf(TdxBody.adis_timeleft));
  WriteLn('adis_stepsleft: ', PtrUInt(@TdxBody(Nil^).adis_stepsleft),'  : ',sizeOf(TdxBody.adis_stepsleft));
  WriteLn('average_lvel_buffer: ', PtrUInt(@TdxBody(Nil^).average_lvel_buffer),'  : ',sizeOf(TdxBody.average_lvel_buffer));
  WriteLn('average_avel_buffer: ', PtrUInt(@TdxBody(Nil^).average_avel_buffer),'  : ',sizeOf(TdxBody.average_avel_buffer));
  WriteLn('average_counter: ', PtrUInt(@TdxBody(Nil^).average_counter),'  : ',sizeOf(TdxBody.average_counter));
  WriteLn('average_ready: ', PtrUInt(@TdxBody(Nil^).average_ready),'  : ',sizeOf(TdxBody.average_ready));
  //WriteLn('moved_callback: ', PtrUInt(@TdxBody(Nil^).moved_callback),'  : ',sizeOf(TdxBody.moved_callback));
  WriteLn('dampingp: ', PtrUInt(@TdxBody(Nil^).dampingp),'  : ',sizeOf(TdxBody.dampingp));
  WriteLn('max_angular_speed: ', PtrUInt(@TdxBody(Nil^).max_angular_speed),'  : ',sizeOf(TdxBody.max_angular_speed));
  WriteLn('sizeof: ', sizeOf(TdxBody));
  WriteLn();
  WriteLn();
  WriteLn('TdxWorld');
  WriteLn('firstbody: ', PtrUInt(@TdxWorld(Nil^).firstbody),'  : ',sizeOf(TdxWorld.firstbody));
  WriteLn('firstjoint: ', PtrUInt(@TdxWorld(Nil^).firstjoint),'  : ',sizeOf(TdxWorld.firstjoint));
  WriteLn('nb: ', PtrUInt(@TdxWorld(Nil^).nb),'  : ',sizeOf(TdxWorld.nb));
  WriteLn('nj: ', PtrUInt(@TdxWorld(Nil^).nj),'  : ',sizeOf(TdxWorld.nj));
  WriteLn('gravity: ', PtrUInt(@TdxWorld(Nil^).gravity),'  : ',sizeOf(TdxWorld.gravity));
  WriteLn('global_erp: ', PtrUInt(@TdxWorld(Nil^).global_erp),'  : ',sizeOf(TdxWorld.global_erp));
  WriteLn('global_cfm: ', PtrUInt(@TdxWorld(Nil^).global_cfm),'  : ',sizeOf(TdxWorld.global_cfm));
  WriteLn('adis: ', PtrUInt(@TdxWorld(Nil^).adis),'  : ',sizeOf(TdxWorld.adis));
  WriteLn('body_flags: ', PtrUInt(@TdxWorld(Nil^).body_flags),'  : ',sizeOf(TdxWorld.body_flags));
  WriteLn('islands_max_threads: ', PtrUInt(@TdxWorld(Nil^).islands_max_threads),'  : ',sizeOf(TdxWorld.islands_max_threads));
  WriteLn('wmem: ', PtrUInt(@TdxWorld(Nil^).wmem),'  : ',sizeOf(TdxWorld.wmem));
  WriteLn('qs: ', PtrUInt(@TdxWorld(Nil^).qs),'  : ',sizeOf(TdxWorld.qs));
  WriteLn('contactp: ', PtrUInt(@TdxWorld(Nil^).contactp),'  : ',sizeOf(TdxWorld.contactp));
  WriteLn('dampingp: ', PtrUInt(@TdxWorld(Nil^).dampingp),'  : ',sizeOf(TdxWorld.dampingp));
  WriteLn('max_angular_speed: ', PtrUInt(@TdxWorld(Nil^).max_angular_speed),'  : ',sizeOf(TdxWorld.max_angular_speed));
  WriteLn('sizeof: ', sizeOf(TdxWorld));
  WriteLn();
  WriteLn();
  WriteLn('TdJointFeedback');
  WriteLn('f1: ', PtrUInt(@TdJointFeedback(Nil^).f1),'  : ',sizeOf(TdJointFeedback.f1));
  WriteLn('t1: ', PtrUInt(@TdJointFeedback(Nil^).t1),'  : ',sizeOf(TdJointFeedback.t1));
  WriteLn('f2: ', PtrUInt(@TdJointFeedback(Nil^).f2),'  : ',sizeOf(TdJointFeedback.f2));
  WriteLn('t2: ', PtrUInt(@TdJointFeedback(Nil^).t2),'  : ',sizeOf(TdJointFeedback.t2));
  WriteLn('sizeof: ', sizeOf(TdJointFeedback));
  WriteLn();
  WriteLn();
  WriteLn('TdSurfaceParameters');
  WriteLn('mode: ', PtrUInt(@TdSurfaceParameters(Nil^).mode),'  : ',sizeOf(TdSurfaceParameters.mode));
  WriteLn('mu: ', PtrUInt(@TdSurfaceParameters(Nil^).mu),'  : ',sizeOf(TdSurfaceParameters.mu));
  WriteLn('mu2: ', PtrUInt(@TdSurfaceParameters(Nil^).mu2),'  : ',sizeOf(TdSurfaceParameters.mu2));
  WriteLn('rho: ', PtrUInt(@TdSurfaceParameters(Nil^).rho),'  : ',sizeOf(TdSurfaceParameters.rho));
  WriteLn('rho2: ', PtrUInt(@TdSurfaceParameters(Nil^).rho2),'  : ',sizeOf(TdSurfaceParameters.rho2));
  WriteLn('rhoN: ', PtrUInt(@TdSurfaceParameters(Nil^).rhoN),'  : ',sizeOf(TdSurfaceParameters.rhoN));
  WriteLn('bounce: ', PtrUInt(@TdSurfaceParameters(Nil^).bounce),'  : ',sizeOf(TdSurfaceParameters.bounce));
  WriteLn('bounce_vel: ', PtrUInt(@TdSurfaceParameters(Nil^).bounce_vel),'  : ',sizeOf(TdSurfaceParameters.bounce_vel));
  WriteLn('soft_erp: ', PtrUInt(@TdSurfaceParameters(Nil^).soft_erp),'  : ',sizeOf(TdSurfaceParameters.soft_erp));
  WriteLn('soft_cfm: ', PtrUInt(@TdSurfaceParameters(Nil^).soft_cfm),'  : ',sizeOf(TdSurfaceParameters.soft_cfm));
  WriteLn('motion1: ', PtrUInt(@TdSurfaceParameters(Nil^).motion1),'  : ',sizeOf(TdSurfaceParameters.motion1));
  WriteLn('motion2: ', PtrUInt(@TdSurfaceParameters(Nil^).motion2),'  : ',sizeOf(TdSurfaceParameters.motion2));
  WriteLn('motionN: ', PtrUInt(@TdSurfaceParameters(Nil^).motionN),'  : ',sizeOf(TdSurfaceParameters.motionN));
  WriteLn('slip1: ', PtrUInt(@TdSurfaceParameters(Nil^).slip1),'  : ',sizeOf(TdSurfaceParameters.slip1));
  WriteLn('slip2: ', PtrUInt(@TdSurfaceParameters(Nil^).slip2),'  : ',sizeOf(TdSurfaceParameters.slip2));
  WriteLn('sizeof: ', sizeOf(TdSurfaceParameters));
  WriteLn();
  WriteLn();
  WriteLn('TdContactGeom');
  WriteLn('pos: ', PtrUInt(@TdContactGeom(Nil^).pos),'  : ',sizeOf(TdContactGeom.pos));
  WriteLn('normal: ', PtrUInt(@TdContactGeom(Nil^).normal),'  : ',sizeOf(TdContactGeom.normal));
  WriteLn('depth: ', PtrUInt(@TdContactGeom(Nil^).depth),'  : ',sizeOf(TdContactGeom.depth));
  WriteLn('g1: ', PtrUInt(@TdContactGeom(Nil^).g1),'  : ',sizeOf(TdContactGeom.g1));
  WriteLn('g2: ', PtrUInt(@TdContactGeom(Nil^).g2),'  : ',sizeOf(TdContactGeom.g2));
  WriteLn('side1: ', PtrUInt(@TdContactGeom(Nil^).side1),'  : ',sizeOf(TdContactGeom.side1));
  WriteLn('side2: ', PtrUInt(@TdContactGeom(Nil^).side2),'  : ',sizeOf(TdContactGeom.side2));
  WriteLn('sizeof: ', sizeOf(TdContactGeom));
  WriteLn();
  WriteLn();
  WriteLn('TdContact');
  WriteLn('surface: ', PtrUInt(@TdContact(Nil^).surface),'  : ',sizeOf(TdContact.surface));
  WriteLn('geom: ', PtrUInt(@TdContact(Nil^).geom),'  : ',sizeOf(TdContact.geom));
  WriteLn('fdir1: ', PtrUInt(@TdContact(Nil^).fdir1),'  : ',sizeOf(TdContact.fdir1));
  WriteLn('sizeof: ', sizeOf(TdContact));
  WriteLn();
  WriteLn();
  WriteLn('TdGeomClass');
  WriteLn('bytes: ', PtrUInt(@TdGeomClass(Nil^).bytes),'  : ',sizeOf(TdGeomClass.bytes));
  WriteLn('collider: -','  : ',sizeOf(TdGeomClass.collider));
  WriteLn('aabb: -','  : ',sizeOf(TdGeomClass.aabb));
  WriteLn('aabb_test: -', '  : ',sizeOf(TdGeomClass.aabb_test));
  WriteLn('dtor: -', '  : ',sizeOf(TdGeomClass.dtor));
  WriteLn('sizeof: ', sizeOf(TdGeomClass));
  WriteLn();
  WriteLn();
  WriteLn('TdxTriMeshData');
  WriteLn('unknown: ', PtrUInt(@TdxTriMeshData(Nil^).unknown),'  : ',sizeOf(TdxTriMeshData.unknown));
  WriteLn('sizeof: ', sizeOf(TdxTriMeshData));
  WriteLn();
  WriteLn();
  WriteLn('TdxHeightfieldData');
  WriteLn('m_fWidth: ', PtrUInt(@TdxHeightfieldData(Nil^).m_fWidth),'  : ',sizeOf(TdxHeightfieldData.m_fWidth));
  WriteLn('m_fDepth: ', PtrUInt(@TdxHeightfieldData(Nil^).m_fDepth),'  : ',sizeOf(TdxHeightfieldData.m_fDepth));
  WriteLn('m_fSampleWidth: ', PtrUInt(@TdxHeightfieldData(Nil^).m_fSampleWidth),'  : ',sizeOf(TdxHeightfieldData.m_fSampleWidth));
  WriteLn('m_fSampleDepth: ', PtrUInt(@TdxHeightfieldData(Nil^).m_fSampleDepth),'  : ',sizeOf(TdxHeightfieldData.m_fSampleDepth));
  WriteLn('m_fSampleZXAspect: ', PtrUInt(@TdxHeightfieldData(Nil^).m_fSampleZXAspect),'  : ',sizeOf(TdxHeightfieldData.m_fSampleZXAspect));
  WriteLn('m_fInvSampleWidth: ', PtrUInt(@TdxHeightfieldData(Nil^).m_fInvSampleWidth),'  : ',sizeOf(TdxHeightfieldData.m_fInvSampleWidth));
  WriteLn('m_fInvSampleDepth: ', PtrUInt(@TdxHeightfieldData(Nil^).m_fInvSampleDepth),'  : ',sizeOf(TdxHeightfieldData.m_fInvSampleDepth));
  WriteLn('m_fHalfWidth: ', PtrUInt(@TdxHeightfieldData(Nil^).m_fHalfWidth),'  : ',sizeOf(TdxHeightfieldData.m_fHalfWidth));
  WriteLn('m_fHalfDepth: ', PtrUInt(@TdxHeightfieldData(Nil^).m_fHalfDepth),'  : ',sizeOf(TdxHeightfieldData.m_fHalfDepth));
  WriteLn('m_fMinHeight: ', PtrUInt(@TdxHeightfieldData(Nil^).m_fMinHeight),'  : ',sizeOf(TdxHeightfieldData.m_fMinHeight));
  WriteLn('m_fMaxHeight: ', PtrUInt(@TdxHeightfieldData(Nil^).m_fMaxHeight),'  : ',sizeOf(TdxHeightfieldData.m_fMaxHeight));
  WriteLn('m_fThickness: ', PtrUInt(@TdxHeightfieldData(Nil^).m_fThickness),'  : ',sizeOf(TdxHeightfieldData.m_fThickness));
  WriteLn('m_fScale: ', PtrUInt(@TdxHeightfieldData(Nil^).m_fScale),'  : ',sizeOf(TdxHeightfieldData.m_fScale));
  WriteLn('m_fOffset: ', PtrUInt(@TdxHeightfieldData(Nil^).m_fOffset),'  : ',sizeOf(TdxHeightfieldData.m_fOffset));
  WriteLn('m_nWidthSamples: ', PtrUInt(@TdxHeightfieldData(Nil^).m_nWidthSamples),'  : ',sizeOf(TdxHeightfieldData.m_nWidthSamples));
  WriteLn('m_nDepthSamples: ', PtrUInt(@TdxHeightfieldData(Nil^).m_nDepthSamples),'  : ',sizeOf(TdxHeightfieldData.m_nDepthSamples));
  WriteLn('m_bCopyHeightData: ', PtrUInt(@TdxHeightfieldData(Nil^).m_bCopyHeightData),'  : ',sizeOf(TdxHeightfieldData.m_bCopyHeightData));
  WriteLn('m_bWrapMode: ', PtrUInt(@TdxHeightfieldData(Nil^).m_bWrapMode),'  : ',sizeOf(TdxHeightfieldData.m_bWrapMode));
  WriteLn('m_nGetHeightMode: ', PtrUInt(@TdxHeightfieldData(Nil^).m_nGetHeightMode),'  : ',sizeOf(TdxHeightfieldData.m_nGetHeightMode));
  WriteLn('m_pHeightData: ', PtrUInt(@TdxHeightfieldData(Nil^).m_pHeightData),'  : ',sizeOf(TdxHeightfieldData.m_pHeightData));
  WriteLn('m_pUserData: ', PtrUInt(@TdxHeightfieldData(Nil^).m_pUserData),'  : ',sizeOf(TdxHeightfieldData.m_pUserData));
  WriteLn('m_contacts: ', PtrUInt(@TdxHeightfieldData(Nil^).m_contacts),'  : ',sizeOf(TdxHeightfieldData.m_contacts));
  WriteLn('sizeof: ', sizeOf(TdxHeightfieldData));
  WriteLn();
  WriteLn();
  //WriteLn('TdGeomSpaceData');
  //WriteLn('next: ', PtrUInt(@TdGeomSpaceData(Nil^).next),'  : ',sizeOf(TdGeomSpaceData.next));
  //WriteLn('sizeof: ', sizeOf(TdGeomSpaceData));
  //WriteLn();
  //WriteLn();
  WriteLn('TdxGeom');
  WriteLn('_type: ', PtrUInt(@TdxGeom(Nil^)._type),'  : ',sizeOf(TdxGeom._type));
  //WriteLn('Padding: ', PtrUInt(@TdxGeom(Nil^).Padding),'  : ',sizeOf(TdxGeom.Padding));
  WriteLn('gflags: ', PtrUInt(@TdxGeom(Nil^).gflags),'  : ',sizeOf(TdxGeom.gflags));
  WriteLn('data: ', PtrUInt(@TdxGeom(Nil^).data),'  : ',sizeOf(TdxGeom.data));
  WriteLn('Body: ', PtrUInt(@TdxGeom(Nil^).Body),'  : ',sizeOf(TdxGeom.Body));
  WriteLn('body_next: ', PtrUInt(@TdxGeom(Nil^).body_next),'  : ',sizeOf(TdxGeom.body_next));
  WriteLn('final_posr: ', PtrUInt(@TdxGeom(Nil^).final_posr),'  : ',sizeOf(TdxGeom.final_posr));
  WriteLn('offset_posr: ', PtrUInt(@TdxGeom(Nil^).offset_posr),'  : ',sizeOf(TdxGeom.offset_posr));
  WriteLn('next: ', PtrUInt(@TdxGeom(Nil^).next),'  : ',sizeOf(TdxGeom.next));
  WriteLn('tome: ', PtrUInt(@TdxGeom(Nil^).tome),'  : ',sizeOf(TdxGeom.tome));
  WriteLn('next_ex: ', PtrUInt(@TdxGeom(Nil^).next_ex),'  : ',sizeOf(TdxGeom.next_ex));
  WriteLn('tome_ex: ', PtrUInt(@TdxGeom(Nil^).tome_ex),'  : ',sizeOf(TdxGeom.tome_ex));
  WriteLn('parent_space: ', PtrUInt(@TdxGeom(Nil^).parent_space),'  : ',sizeOf(TdxGeom.parent_space));
  WriteLn('aabb: ', PtrUInt(@TdxGeom(Nil^).aabb),'  : ',sizeOf(TdxGeom.aabb));
  WriteLn('category_bits: ', PtrUInt(@TdxGeom(Nil^).category_bits),'  : ',sizeOf(TdxGeom.category_bits));
  WriteLn('collide_bits: ', PtrUInt(@TdxGeom(Nil^).collide_bits),'  : ',sizeOf(TdxGeom.collide_bits));
  WriteLn('sizeof: ', sizeOf(TdxGeom));
  WriteLn();
  WriteLn();
  WriteLn('TdxSpace');
  WriteLn('baseGeom: ', PtrUInt(@TdxSpace(Nil^).baseGeom),'  : ',sizeOf(TdxSpace.baseGeom));
  WriteLn('count: ', PtrUInt(@TdxSpace(Nil^).count),'  : ',sizeOf(TdxSpace.count));
  WriteLn('first: ', PtrUInt(@TdxSpace(Nil^).first),'  : ',sizeOf(TdxSpace.first));
  WriteLn('cleanup: ', PtrUInt(@TdxSpace(Nil^).cleanup),'  : ',sizeOf(TdxSpace.cleanup));
  WriteLn('sublevel: ', PtrUInt(@TdxSpace(Nil^).sublevel),'  : ',sizeOf(TdxSpace.sublevel));
  WriteLn('tls_kind: ', PtrUInt(@TdxSpace(Nil^).tls_kind),'  : ',sizeOf(TdxSpace.tls_kind));
  WriteLn('current_index: ', PtrUInt(@TdxSpace(Nil^).current_index),'  : ',sizeOf(TdxSpace.current_index));
  WriteLn('current_geom: ', PtrUInt(@TdxSpace(Nil^).current_geom),'  : ',sizeOf(TdxSpace.current_geom));
  WriteLn('lock_count: ', PtrUInt(@TdxSpace(Nil^).lock_count),'  : ',sizeOf(TdxSpace.lock_count));
  WriteLn('sizeof: ', sizeOf(TdxSpace));
  WriteLn();
  WriteLn();
  WriteLn('TdxHashSpace');
  WriteLn('BaseSpace: ', PtrUInt(@TdxHashSpace(Nil^).BaseSpace),'  : ',sizeOf(TdxHashSpace.BaseSpace));
  WriteLn('global_minlevel: ', PtrUInt(@TdxHashSpace(Nil^).global_minlevel),'  : ',sizeOf(TdxHashSpace.global_minlevel));
  WriteLn('global_maxlevel: ', PtrUInt(@TdxHashSpace(Nil^).global_maxlevel),'  : ',sizeOf(TdxHashSpace.global_maxlevel));
  WriteLn('sizeof: ', sizeOf(TdxHashSpace));
  WriteLn();
  WriteLn();
  WriteLn('TdxQuadTreeSpace');
  WriteLn('BaseSpace: ', PtrUInt(@TdxQuadTreeSpace(Nil^).BaseSpace),'  : ',sizeOf(TdxQuadTreeSpace.BaseSpace));
  WriteLn('Blocks: ', PtrUInt(@TdxQuadTreeSpace(Nil^).Blocks),'  : ',sizeOf(TdxQuadTreeSpace.Blocks));
  WriteLn('DirtyList: ', PtrUInt(@TdxQuadTreeSpace(Nil^).DirtyList),'  : ',sizeOf(TdxQuadTreeSpace.DirtyList));
  WriteLn('sizeof: ', sizeOf(TdxQuadTreeSpace));
  WriteLn();
  WriteLn('printed');
end;

//---------------------------------------
initialization
//---------------------------------------

 InitODE(ODEDLL);
 //jm_print_dll_sizes();
//---------------------------------------
finalization
//---------------------------------------

   CloseODE;

end.

module VisProg.DataDriven
open System
open FSharp.Core
type multiplyInputs={a:int; b:int}
type Result1< 'a> ={R1:'a}
type Result2< 'a,'b> ={R1:'a;R2:'b}
type Input1< 'a> ={I1:'a}
type Input2< 'a, 'b> ={I1:'a; I2: 'b}

type IDoubleInNode<'x,'y, 'z> =
    abstract member exec :Input2< 'x, 'y> -> 'z
type IDoubleOutNode<'x,'y, 'z> =
    abstract member exec :'x -> Result2<'y,'z>
type ISingleNode<'x,'y> =
    abstract member exec :Input1<'x> -> 'y
let inline (!|)(r)=
    {R1=r}
let inline (!||)(r,r2)=
    {R1=r;R2=r2}

type DoubleDoubleNode<'a,'b,'c,'d>(desc:string,func:Input2< 'a,'b> ->Result2< 'c,'d>) =
    member val description=desc
    member x.exec =func
    interface IDoubleInNode<'a,'b, Result2< 'c,'d> > with
        member x.exec a=func a
    interface IDoubleOutNode<Input2< 'a,'b> ,'c,'d > with
        member x.exec a=func a

type SingleDoubleNode<'a,'c,'d>(desc:string,func:Input1< 'a> ->Result2< 'c,'d>) =
    member val description=desc
    member x.exec =func
    interface ISingleNode<'a, Result2< 'c,'d> > with
        member x.exec a=func a
    interface IDoubleOutNode<Input1< 'a> ,'c,'d > with
        member x.exec a=func a

let multiandPlus  (a:Input2<int,int>):Result2<int,int> =
    let multiplied=a.I1*a.I2
    {R1=multiplied;R2=a.I1+a.I2}
let StringPlus  (a:Input2<string,string>):Result2<string,string> =
    let added=a.I1+a.I2
    {R1=added;R2=a.I1}

let a= DoubleDoubleNode("multiplus",multiandPlus)
let b= DoubleDoubleNode("mult",StringPlus)
let add1= SingleDoubleNode("adds one",(fun x-> !||(x.I1+1,x.I1) ))

type MultiplyNode = {
    description:string
    
}
// iterate through the outputs and create a mapping from the inputof one thing to the output of anther
(* type Node<'a,'b>(description:string, input) = 

    member val description=description
    member inputs 'a=
 *)

type Nodes=
    |MultiPlyNode of MultiplyNode

let executor(nodes:Nodes)=
 match nodes with 
 |Nodes.MultiPlyNode(a)-> (fun (x: multiplyInputs)->  {|Result = x.a+x.b|})

//let inline isOld (x:'T) = ('T : (member Age : int) x) > = 65

(* let inline get2outtput (x:'T) = 
    let a=( 'T : (member R2 : 'a) x ) 

let inline get1outtput (x:'T) = 
    ( 'T : (member R1 : 'a) x )    *)  
    
let create2Input in1 in2 :Input2<'a,'b> =
    {I1=in1();I2=in2() } 

module Decode
open System
open System.IO

let _lg27 = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/="

let rec ins (a: _ ResizeArray) defaultVal i value = 
    if i < a.Count then a.[i] <- value
    else 
        a.Add defaultVal
        ins a defaultVal i value

let rec get (a: _ ResizeArray) defaultVal i =
    if i < a.Count then a.[i]
    else 
        a.Add defaultVal
        get a defaultVal i
let encodeByteArray(param1:MemoryStream) =
        
//        var _loc_3:* = null;
    let mutable _loc_3 = null
//        var _loc_5:* = 0;
    let mutable _loc_5 = 0
//        var _loc_6:* = 0;
    let mutable _loc_6 = 0
//        var _loc_7:* = 0;
    let mutable _loc_7 = 0
//        var _loc_2:* = "";
    let mutable _loc_2 = ""
//        var _loc_4:* = new Array(4);
    let mutable _loc_4 = Array.create 4 0
//        param1.position = 0;
    param1.Position <- 0L

//        while (param1.bytesAvailable > 0)
//        {
    while param1.Position <> param1.Length do
//            _loc_3 = new Array();
        let _loc_3 = ResizeArray<int>()
        let add = ins _loc_3 0
        let get = get _loc_3 0

//            _loc_5 = 0;
//            while (_loc_5 < 3 && param1.bytesAvailable > 0)
//            {
        _loc_5 <- 0
        while _loc_5 < 3 && param1.Position <> param1.Length do
//                _loc_3[_loc_5] = param1.readUnsignedByte();
//                _loc_5 = _loc_5 + 1;
            add _loc_5 <| param1.ReadByte()
            _loc_5 <- _loc_5 + 1

//            _loc_4[0] = (_loc_3[0] & 252) >> 2;
        _loc_4.[0] <- ((get 0 &&& 252) >>> 2)
//            _loc_4[1] = (_loc_3[0] & 3) << 4 | _loc_3[1] >> 4;
        _loc_4.[1] <- ((get 0 &&& 3) <<< 4 ||| get 1 >>> 4)
//            _loc_4[2] = (_loc_3[1] & 15) << 2 | _loc_3[2] >> 6;
        _loc_4.[2] <- ((get 1 &&& 15) <<< 2 ||| get 2 >>> 6)
//            _loc_4[3] = _loc_3[2] & 63;
        _loc_4.[3] <- (get 2 &&& 63)
//            _loc_6 = _loc_3.length;
        _loc_6 <- _loc_3.Count
            
//            while (_loc_6 < 3)
        while _loc_6 < 3 do
//                _loc_4[(_loc_6 + 1)] = 64;
//                _loc_6 = _loc_6 + 1;
            _loc_4.[_loc_6 + 1] <- 64;
            _loc_6 <- _loc_6 + 1;

//            _loc_7 = 0;
//            while (_loc_7 < _loc_4.length)
//            {
        _loc_7 <- 0
        while _loc_7 < _loc_4.Length do
//                    
//                _loc_2 = _loc_2 + _lg27.charAt(_loc_4[_loc_7]);
//                _loc_7 = _loc_7 + 1;
            _loc_2 <- _loc_2 + string _lg27.[_loc_4.[_loc_7]]
            _loc_7 <- _loc_7 + 1;
//        return _loc_2;
    _loc_2
let K12K = failwith ""
let _off(param1:String) =
    //var _loc_2:* = new ByteArray();
    //_loc_2.writeUTFBytes(param1);
    assert
        let param1 = "2iob3gRLvabM0QEMtgTM0ake6Hm=2xmLkasBUaWzUHJZkQDbtdy4tQEbkQTtQ3NWkQnctxtbUju5kQUmt5TNUQD1UwJPwEwzQx7o6ZWqoQwPT1omQwowPSJ52xWM3NJR3QYLGdFm05FmUSze2d4e0HzZ0azh3am72Cze2d4a0coB6xDMtjAWyHm50iUr"
        true
    let en = System.Text.UTF8Encoding()
    let _loc_2 = en.GetBytes(param1)
    let _loc_2' = new MemoryStream(_loc_2)

    //var _loc_3:* = encodeByteArray(_loc_2);
    let _loc_3 = encodeByteArray(_loc_2')
    //_loc_3 = K12K(_loc_3, "e");
    //return _loc_3;
    K12K(_loc_3, "e");
    ()
open Qml.Net
open Qml.Net.Runtimes
open System

// For more information see https://aka.ms/fsharp-console-apps
printfn "Hello from F#"
[<EntryPoint>]
let main(args)=
    RuntimeManager.DiscoverOrDownloadSuitableQtRuntime()
    QQuickStyle.SetStyle("Material")
    use application = new QGuiApplication(args)
            
    use  qmlEngine = new QQmlApplicationEngine()

    Qml.Net.Qml.RegisterType<SignalsModel>("Features");
    Qml.Net.Qml.RegisterType<NotifySignalsModel>("Features");
    Qml.Net.Qml.RegisterType<AsyncAwaitModel>("Features");
    Qml.Net.Qml.RegisterType<NetObjectsModel>("Features");
    Qml.Net.Qml.RegisterType<DynamicModel>("Features");
    Qml.Net.Qml.RegisterType<CalculatorModel>("Features");
    Qml.Net.Qml.RegisterType<CollectionsModel>("Features");

    qmlEngine.Load("Main.qml");
    
    application.Exec()

    
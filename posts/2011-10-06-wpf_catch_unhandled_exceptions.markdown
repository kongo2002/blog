---
tags: c#, .net, wpf
title: WPF: How to catch unhandled exceptions
author: Gregor Uhlenheuer
summary: In order to catch unhandled exceptions in WPF applications you are not
         able to hook into the event you are used to in WinForms - now there
         are even two events you can listen to.
---
In order to catch unhandled exceptions in WPF applications you are not able to
hook into the event you are used to from WinForms - now there are even three
levels you can listen for unhandled exceptions on.

## UnhandledException

The first event you can attach to is the `UnhandledException` event of the
current application domain. See the [documentation][1] on the msdn for more
information.

~~~ {.cs}
AppDomain currentDomain = AppDomain.CurrentDomain;
currentDomain.UnhandledException +=
    new UnhandledExceptionEventHandler(CustomHandler);

// ...

private static void CustomHandler(object sender, UnhandledExceptionEventArgs e)
{
    Exception ex = (Exception) e.ExceptionObject;
    Console.WriteLine("Caught unhandled exception: " + ex.Message);
}
~~~

## DispatcherUnhandledException

The event `DispatcherUnhandledException` is triggered by the main UI
dispatcher of your WPF application. The [documentation][2] can be found on the
msdn.

~~~ {.cs}
Application.Current.DispatcherUnhandledException +=
    new DispatcherUnhandledExceptionEventHandler(CustomHandler);

// ...

private static void CustomHandler(object sender, DispatcherUnhandledExceptionEventArgs e)
{
    Console.WriteLine("Caught unhandled exception: " + e.Exception.Message);
}
~~~

Moreover it is possible to hook into the `DispatcherUnhandledException` event
of a specific Dispatcher instance. The behavior is described in the
[documentation][3] on the msdn in more detail.

~~~ {.cs}
dispatcher.DispatcherUnhandledException +=
    new DispatcherUnhandledExceptionEventHandler(CustomHandler);
~~~

### References

- [AppDomain.UnhandledException][1] on msdn
- [Application.DispatcherUnhandledException][2] on msdn
- [Dispatcher.DispatcherUnhandledException][3] on msdn

[1]: http://msdn.microsoft.com/en-us/library/system.appdomain.unhandledexception.aspx
[2]: http://msdn.microsoft.com/en-us/library/system.windows.application.dispatcherunhandledexception.aspx
[3]: http://msdn.microsoft.com/en-us/library/system.windows.threading.dispatcher.unhandledexception.aspx

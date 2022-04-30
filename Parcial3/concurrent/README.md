# Elixir

Elixir es un lenguaje de programacion que posee concurrencia nativa, en parte esto es de esperarse, puesto que el compila a la BEAM VM, la cual es la maquina virtual que corre ERLANG.

La concurrencia en Elixir (y toda la familia que trabaja sobre BEAM) implementa concurrencia mediante "procesos", los cuales son en realidad hilos super ligeros con un overhead de espacio minimo.

La manera en que se comunican estos "procesos" es mediante el pasaje de mensajes (los cuales estan traducidos de manera casi literal! el conjunto de instrucciones de BEAM es magico): cada proceso tiene su propia cola de mensajes locales, mientras que para mandar un mensaje, se copia este en la cola de recibidos del proceso destinario, pero si ningun mensaje ha sido recibido, y el proceso pide uno, entonces este es swapeado por el scheduler hasta que o un proceso mande un mensaje e interrumpa, o ocurra un time out.

Ahora, la sincronizacion del paso de mensajes en realidad viene por atomicidad, en ensamblador de alto nivel tenemos que el `recieve` se comporta como:


```elixir
rec:  Rec (set a message pointer)
   loop:  loopRec (fetch the message)
pattern:  match the message against required pattern
          if matched then
               RmMsg (remove the message from the mailbox,
                       mark all other messages as untried,
                       if the timeout has been set then
                          clear the timeout)
               evaluate given action
               TPattEnd (goto next instruction after receive)
          else
               if there are more patterns to be matched then
                    goto pattern (try the next pattern)
               else
                    loopRecEnd (mark the current message as tried,
                                if there are more messages then
                                   update message pointer,
                                   goto loop (try the next message))
                    WaitTOut (if there is given timeout and 
                               the timeout is not set
                                 set the required timeout and suspend)
                    Wait (suspend)
```

Notemos que la seccion critica pasa cuando llegamos a `RmMsg`, la cual [Tiene una instruccion probablemente atomica para esto!](http://www.cs-lab.org/historical_beam_instruction_set.html#2.9%20Inter-process%20Communication), lo mismo pasa para el envio de mensajes.

## Programas!

Las respuestas estan en: [./lib/concurrent.ex](./lib/concurrent.ex)
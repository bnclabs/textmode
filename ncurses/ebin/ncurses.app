{application, ncurses,
    [ {description,  "NCurses interface"},
      {vsn,          "0.1.0"},
      {modules,      [ncurses, ncdrv, ncnode, ncbuf, ncdom, nc_examples]},
      {registered,   [ncurses, ncdrv]},
      {mod,          {ncurses, []}},
      {env,          [{childspec,
                           {{one_for_one, 10, 10},
                            [{ncdrv,
                              {ncdrv, start_link, [[]]},
                              permanent,
                              5000,
                              worker,
                              [ncdrv]}
                            ]}}
                     ]},
      {applications, [kernel, stdlib]}
    ]
}.

% vim: filetype=erlang:

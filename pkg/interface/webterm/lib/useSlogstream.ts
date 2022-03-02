import { useCallback, useEffect } from 'react';
import useTermState, { Session } from 'state';
import { Terminal } from 'xterm';
import { csi } from './csi';

// NOTE  should generally only be passed the default terminal session
const showSlog = (term: Terminal, slog: string) => {
  //  set scroll region to exclude the bottom line,
  //  scroll up one line,
  //  move cursor to start of the newly created whitespace,
  //  set text to grey,
  //  print the slog,
  //  restore color, scroll region, and cursor.
  //
  term.write(csi('r', 1, term.rows - 1)
           + csi('S', 1)
           + csi('H', term.rows - 1, 1)
           + csi('m', 90)
           + slog
           + csi('m', 0)
           + csi('r')
           + csi('u'));
};

export const useSlogstream = (session: Session) => {
  // TODO: it looks like the state's slogstream
  // was originally implemented with the assumption
  // that there is only one session; we may need to
  // push it to the Session (i.e, each session has its
  // own stream)
  const slogstream = useTermState(s => s.slogstream);

  const setupSlog = useCallback(() => {
    console.log('slog: setting up...');
    let available = false;
    const slog = new EventSource('/~_~/slog', { withCredentials: true });

    slog.onopen = (e) => {
      console.log('slog: opened stream');
      available = true;
    };

    slog.onmessage = (e) => {
      const session = useTermState.getState().sessions[''];
      if (!session) {
        console.log('default session mia!', 'slog:', slog);
        return;
      }
      showSlog(session.term, e.data);
    };

    slog.onerror = (e) => {
      console.error('slog: eventsource error:', e);
      if (available) {
        window.setTimeout(() => {
          if (slog.readyState !== EventSource.CLOSED) {
            return;
          }
          console.log('slog: reconnecting...');
          setupSlog();
        }, 10000);
      }
    };

    useTermState.getState().set((state) => {
      state.slogstream = slog;
    });
  }, []);

  useEffect(() => {
    if (!slogstream) {
      setupSlog();
    }
  });
};

import { useState, useEffect } from 'react';

export function useIdlingState() {
  const [idling, setIdling] = useState(false);

  useEffect(() => { 
    function blur() {
      setIdling(true);
    }
    function focus() {
      setIdling(false);
    }
    window.addEventListener('blur', blur);
    window.addEventListener('focus', focus);

    return () => {
      window.removeEventListener('blur', blur);
      window.removeEventListener('focus', focus);
    }
  }, []);

  return idling;
}

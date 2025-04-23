import React, {
    useEffect,
    useRef, useCallback,
    useState} from 'react';


const Details = ({ ...props }) => {
    const detailsRef = useRef();
    const onToggleCallback = useCallback(() => {
      props.onToggle(detailsRef.current?.open)
    }, [props.onToggle]);

    useEffect(() => {
      detailsRef.current?.addEventListener('toggle', onToggleCallback)

      return () => {
        detailsRef.current?.removeEventListener('toggle', onToggleCallback)
      }
    }, [onToggleCallback])


    return (
      <details ref={detailsRef} open={props.open}
        style={{border: '1px solid black', padding: '4px', position: 'relative', ...props.style}}
      >
        <summary>{props.summary}</summary>
        <div style={{borderTop: '1px solid black'}}>{props.details}</div>
      </details>
    );
}

export const Summary = ({ ...props }) => {
  const [isOpen, setIsOpen] = useState(false);

  return (
    <Details open={isOpen} onToggle={(value) => {
        setIsOpen(value);
        if (props.onOpen) {
          props.onOpen(props.id);
        }
      }
    } {...props} />
  )
}

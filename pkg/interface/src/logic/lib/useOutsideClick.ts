import { useEffect, RefObject } from "react";

export function useOutsideClick(
  ref: RefObject<HTMLElement>,
  onClick: () => void
) {
  useEffect(() => {
    function handleClick(event: MouseEvent) {
      if (
        ref.current &&
        !ref.current.contains(event.target as any) &&
        !document.querySelector("#portal-root")!.contains(event.target as any)
      ) {
        onClick();
      }
    }
    document.addEventListener("mousedown", handleClick);

    return () => {
      document.removeEventListener("mousedown", handleClick);
    };
  }, [ref.current, onClick]);
}

import classNames from 'classnames';
import React, { useState } from 'react';
import * as RadixToggle from '@radix-ui/react-toggle';
import type * as Polymorphic from '@radix-ui/react-polymorphic';

type ToggleComponent = Polymorphic.ForwardRefComponent<
  Polymorphic.IntrinsicElement<typeof RadixToggle.Root>,
  Polymorphic.OwnProps<typeof RadixToggle.Root> & {
    loading?: boolean;
    toggleClass?: string;
    knobClass?: string;
  }
>;

export const Toggle = React.forwardRef(
  (
    { defaultPressed, pressed, onPressedChange, disabled, className, toggleClass, loading = false },
    ref
  ) => {
    const [on, setOn] = useState(defaultPressed);
    const isControlled = !!onPressedChange;
    const proxyPressed = isControlled ? pressed : on;
    const proxyOnPressedChange = isControlled ? onPressedChange : setOn;
    const knobPosition = proxyPressed ? 16 : 8;

    return (
      <RadixToggle.Root
        className={classNames('default-ring rounded-full', className)}
        pressed={proxyPressed}
        onPressedChange={proxyOnPressedChange}
        disabled={disabled || loading}
        ref={ref}
      >
        <svg
          className={classNames('w-6 h-6', toggleClass)}
          fill="none"
          xmlns="http://www.w3.org/2000/svg"
          viewBox="0 0 24 24"
        >
          <rect
            className={classNames(
              'fill-current',
              disabled && proxyPressed && 'text-gray-700',
              !proxyPressed && 'text-gray-200'
            )}
            y="4"
            width="24"
            height="16"
            rx="8"
          />
          <circle
            className={classNames('fill-current text-white', disabled && 'opacity-60')}
            cx={knobPosition}
            cy="12"
            r="6"
          />
        </svg>
      </RadixToggle.Root>
    );
  }
) as ToggleComponent;

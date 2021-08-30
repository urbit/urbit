import classNames from 'classnames';
import React, { useState } from 'react';
import * as RadixToggle from '@radix-ui/react-toggle';
import type * as Polymorphic from '@radix-ui/react-polymorphic';

type ToggleComponent = Polymorphic.ForwardRefComponent<
  Polymorphic.IntrinsicElement<typeof RadixToggle.Root>,
  Polymorphic.OwnProps<typeof RadixToggle.Root> & {
    knobClass?: string;
  }
>;

export const Toggle = React.forwardRef(
  ({ defaultPressed, pressed, onPressedChange, disabled, className }, ref) => {
    const [on, setOn] = useState(defaultPressed);
    const isControlled = !!onPressedChange;
    const proxyPressed = isControlled ? pressed : on;
    const proxyOnPressedChange = isControlled ? onPressedChange : setOn;
    const knobPosition = proxyPressed ? 18 : 2;

    return (
      <RadixToggle.Root
        className="default-ring rounded-full"
        pressed={proxyPressed}
        onPressedChange={proxyOnPressedChange}
        disabled={disabled}
        ref={ref}
      >
        <svg
          className={classNames('w-12 h-8', className)}
          viewBox="0 0 48 32"
          fill="none"
          xmlns="http://www.w3.org/2000/svg"
        >
          <path
            className={classNames(
              'fill-current',
              disabled && proxyPressed && 'text-gray-700',
              !proxyPressed && 'text-gray-200'
            )}
            d="M0 16C0 7.16344 7.16344 0 16 0H32C40.8366 0 48 7.16344 48 16C48 24.8366 40.8366 32 32 32H16C7.16344 32 0 24.8366 0 16Z"
          />
          <rect
            className={classNames('fill-current text-white', disabled && 'opacity-60')}
            x={knobPosition}
            y="2"
            width="28"
            height="28"
            rx="14"
          />
        </svg>
      </RadixToggle.Root>
    );
  }
) as ToggleComponent;

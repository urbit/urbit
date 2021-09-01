import React from 'react';
import type * as Polymorphic from '@radix-ui/react-polymorphic';
import classNames from 'classnames';

type ButtonVariant =
  | 'primary'
  | 'secondary'
  | 'caution'
  | 'destructive'
  | 'alt-primary'
  | 'alt-secondary';

type PolymorphicButton = Polymorphic.ForwardRefComponent<
  'button',
  {
    variant?: ButtonVariant;
  }
>;

const variants: Record<ButtonVariant, string> = {
  primary: 'text-white bg-black',
  secondary: 'text-black bg-gray-100',
  caution: 'text-white bg-orange-400',
  destructive: 'text-white bg-red-500',
  'alt-primary': 'text-white bg-blue-400',
  'alt-secondary': 'text-blue-400 bg-blue-50'
};

export const Button = React.forwardRef(
  ({ as: Comp = 'button', variant = 'primary', children, className, ...props }, ref) => {
    return (
      <Comp
        ref={ref}
        {...props}
        className={classNames('button default-ring', variants[variant], className)}
      >
        {children}
      </Comp>
    );
  }
) as PolymorphicButton;

export const PillButton = React.forwardRef(({ className, children, ...props }, ref) => (
  <Button
    ref={ref}
    {...props}
    className={classNames('px-4 py-2 sm:px-6 sm:py-3 text-sm sm:text-base rounded-full', className)}
  >
    {children}
  </Button>
)) as PolymorphicButton;

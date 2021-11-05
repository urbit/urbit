import React, { FC } from 'react';
import * as DialogPrimitive from '@radix-ui/react-dialog';
import type * as Polymorphic from '@radix-ui/react-polymorphic';
import classNames from 'classnames';

export const Dialog: FC<DialogPrimitive.DialogOwnProps> = ({ children, ...props }) => {
  return (
    <DialogPrimitive.Root {...props}>
      <DialogPrimitive.Overlay className="fixed top-0 bottom-0 left-0 right-0 z-30 bg-black opacity-30" />
      {children}
    </DialogPrimitive.Root>
  );
};

type DialogContentComponent = Polymorphic.ForwardRefComponent<
  Polymorphic.IntrinsicElement<typeof DialogPrimitive.Content>,
  Polymorphic.OwnProps<typeof DialogPrimitive.Content> & {
    containerClass?: string;
    showClose?: boolean;
  }
>;

export const DialogContent = React.forwardRef(
  ({ showClose = true, containerClass, children, className, ...props }, forwardedRef) => (
    <DialogPrimitive.Content
      as="section"
      className={classNames('dialog-container', containerClass)}
      {...props}
      ref={forwardedRef}
    >
      <div className={classNames('dialog', className)}>
        {children}
        {showClose && (
          <DialogPrimitive.Close className="absolute top-4 right-4 sm:top-7 sm:right-7 p-2 bg-gray-100 rounded-full default-ring">
            <svg
              className="w-3.5 h-3.5 stroke-current text-gray-500"
              viewBox="0 0 24 24"
              xmlns="http://www.w3.org/2000/svg"
            >
              <path d="M4 4L20 20" strokeWidth="3" strokeLinecap="round" />
              <path d="M20 4L4 20" strokeWidth="3" strokeLinecap="round" />
            </svg>
          </DialogPrimitive.Close>
        )}
      </div>
    </DialogPrimitive.Content>
  )
) as DialogContentComponent;

export const DialogTrigger = DialogPrimitive.Trigger;
export const DialogClose = DialogPrimitive.Close;

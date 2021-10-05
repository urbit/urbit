import React from 'react';
import cn from 'classnames';
import { Dialog, DialogClose, DialogContent } from './Dialog';
import { Button } from './Button';

interface ErrorAlertProps {
  error: Error;
  resetErrorBoundary: () => void;
  className?: string;
}

const SubmitIssue = ({ error }: { error: Error }) => {
  const title = error.message;
  const body = `\`\`\`%0A${error.stack?.replaceAll('\n', '%0A')}%0A\`\`\``;

  return (
    <Button
      as="a"
      variant="caution"
      href={`https://github.com/urbit/landscape/issues/new?assignees=&labels=bug&title=${title}&body=${body}`}
      target="_blank"
      rel="noreferrer"
    >
      Submit Issue
    </Button>
  );
};

export const ErrorAlert = ({ error, resetErrorBoundary, className }: ErrorAlertProps) => {
  return (
    <Dialog defaultOpen modal onOpenChange={() => resetErrorBoundary()}>
      <DialogContent
        showClose={false}
        className={cn('pr-8 space-y-6', className)}
        containerClass="w-full max-w-3xl"
      >
        <h2 className="h4">
          <span className="mr-3 text-orange-500">Encountered error:</span>
          <span className="font-mono">{error.message}</span>
        </h2>
        {error.stack && (
          <div className="w-full p-2 bg-gray-50 overflow-x-auto rounded">
            <pre>{error.stack}</pre>
          </div>
        )}
        <div className="flex space-x-6">
          <DialogClose as={Button} variant="secondary">
            Try Again
          </DialogClose>
          <DialogClose as={SubmitIssue} error={error} />
        </div>
      </DialogContent>
    </Dialog>
  );
};

import React, { useState } from 'react';
import { useHistory } from 'react-router-dom';
import { Button } from '../components/Button';
import { Checkbox } from '../components/Checkbox';
import { Dialog, DialogContent } from '../components/Dialog';

export const SecurityPrefs = () => {
  const [allSessions, setAllSessions] = useState(false);
  const { push } = useHistory();

  return (
    <Dialog open onOpenChange={(open) => !open && push('/leap/system-preferences')}>
      <DialogContent containerClass="w-1/3" showClose={false}>
        <h3 className="flex items-center mb-2 h4">Log Out</h3>
        <div className="flex flex-col justify-center flex-1 space-y-6 pt-6">
          <p>
            Logging out of Landscape will additionally log you ot of any applications installed on
            your urbit.
          </p>
          <p>You&apos;ll need to log into your urbit again in order to access its apps.</p>
          <Checkbox
            defaultChecked={false}
            checked={allSessions}
            onCheckedChange={() => setAllSessions((prev) => !prev)}
          >
            Log out of all connected sessions.
          </Checkbox>
          <div className="flex space-x-2 justify-end">
            <Button variant="secondary" onClick={() => push('/leap/system-preferences')}>
              Cancel
            </Button>
            <form method="post" action="/~/logout">
              {allSessions && <input type="hidden" name="all" />}
              <Button>Logout</Button>
            </form>
          </div>
        </div>
      </DialogContent>
    </Dialog>
  );
};

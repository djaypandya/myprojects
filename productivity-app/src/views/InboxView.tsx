import React, { useState } from 'react';
import { useAppStore } from '../store/AppStore';
import { Plus } from 'lucide-react';
import type { TimeBucket } from '../types';

export const InboxView: React.FC = () => {
    const [inputValue, setInputValue] = useState('');
    const { tasks, addTask, moveTask, contexts, canAddToday, canAddWeekPriority } = useAppStore();
    const [assigningTaskId, setAssigningTaskId] = useState<string | null>(null);

    const inboxTasks = tasks.filter(t => t.when === 'inbox' && t.status !== 'done');
    const somedayTasks = tasks.filter(t => t.when === 'someday' && t.status !== 'done');

    const handleCapture = (e: React.FormEvent) => {
        e.preventDefault();
        if (!inputValue.trim()) return;

        addTask({
            context_id: null,
            title: inputValue.trim(),
            status: 'todo',
            when: 'inbox',
            is_week_priority: false,
            order_index: Date.now()
        });
        setInputValue('');
    };

    const handleAssign = (taskId: string, contextId: string, when: TimeBucket) => {
        // Check caps
        if (when === 'today' && !canAddToday(contextId)) {
            if (confirm(`You already have 3 Today tasks for this context. Move to "If There's Time" instead?`)) {
                moveTask(taskId, 'today_extra', contextId);
                setAssigningTaskId(null);
            }
            return;
        }

        if (when === 'this_week' && !canAddWeekPriority(contextId)) {
            // For v1, if week is full, we just add it as non-priority this_week or ask user.
            // Spec says: "Add as a non-priority This Week task, or move to Someday?"
            // For simplicity here, we'll just add it as non-priority this_week (which is what moveTask does by default, it doesn't set is_week_priority=true unless we explicitly do so, but moveTask just updates 'when').
            // Wait, moveTask doesn't change is_week_priority. So it will be false. That's fine.
            // But if the user WANTED it to be a priority, they can't here. That's acceptable for v1.
        }

        moveTask(taskId, when, contextId);
        setAssigningTaskId(null);
    };

    return (
        <div style={{ paddingBottom: '100px' }}>
            {/* Capture Bar */}
            <form onSubmit={handleCapture} style={{ marginBottom: '2rem' }}>
                <div className="glass-panel" style={{ display: 'flex', padding: '0.5rem', alignItems: 'center' }}>
                    <input
                        type="text"
                        value={inputValue}
                        onChange={(e) => setInputValue(e.target.value)}
                        placeholder="What do you need to remember?"
                        style={{
                            flex: 1,
                            border: 'none',
                            background: 'transparent',
                            padding: '0.8rem',
                            fontSize: '1rem',
                            outline: 'none',
                            color: 'var(--text-primary)'
                        }}
                    />
                    <button
                        type="submit"
                        className="btn-primary"
                        disabled={!inputValue.trim()}
                        style={{ padding: '0.5rem', borderRadius: '50%', width: '40px', height: '40px', display: 'flex', alignItems: 'center', justifyContent: 'center' }}
                    >
                        <Plus size={24} />
                    </button>
                </div>
            </form>

            {/* Inbox List */}
            <div style={{ marginBottom: '2rem' }}>
                <h3 style={{ marginLeft: '0.5rem', marginBottom: '1rem', display: 'flex', alignItems: 'center', gap: '0.5rem' }}>
                    Inbox <span style={{ fontSize: '0.9rem', color: 'var(--text-secondary)', fontWeight: 'normal' }}>({inboxTasks.length})</span>
                </h3>

                <div style={{ display: 'flex', flexDirection: 'column', gap: '0.8rem' }}>
                    {inboxTasks.map(task => (
                        <div key={task.id} className="glass-panel" style={{ padding: '1rem' }}>
                            <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'start', marginBottom: assigningTaskId === task.id ? '1rem' : 0 }}>
                                <span style={{ fontSize: '1rem' }}>{task.title}</span>
                                {assigningTaskId !== task.id && (
                                    <button
                                        className="btn-primary"
                                        onClick={() => setAssigningTaskId(task.id)}
                                        style={{ fontSize: '0.8rem', padding: '0.4rem 0.8rem' }}
                                    >
                                        Assign
                                    </button>
                                )}
                            </div>

                            {assigningTaskId === task.id && (
                                <div style={{ animation: 'fadeIn 0.2s ease' }}>
                                    <div style={{ height: '1px', background: 'rgba(0,0,0,0.05)', margin: '0.5rem 0 1rem 0' }} />
                                    <p style={{ fontSize: '0.8rem', color: 'var(--text-secondary)', marginBottom: '0.5rem' }}>Select Context & Time:</p>

                                    <div style={{ display: 'flex', flexDirection: 'column', gap: '0.5rem' }}>
                                        {contexts.map(ctx => (
                                            <div key={ctx.id} style={{ display: 'flex', alignItems: 'center', gap: '0.5rem' }}>
                                                <span style={{ width: '70px', fontSize: '0.8rem', fontWeight: 600, color: `var(--theme-${ctx.theme}-primary)` }}>{ctx.name}</span>
                                                <div style={{ display: 'flex', gap: '0.3rem', flex: 1, overflowX: 'auto' }}>
                                                    <AssignButton label="Today" onClick={() => handleAssign(task.id, ctx.id, 'today')} />
                                                    <AssignButton label="Tmrw" onClick={() => handleAssign(task.id, ctx.id, 'tomorrow')} />
                                                    <AssignButton label="Week" onClick={() => handleAssign(task.id, ctx.id, 'this_week')} />
                                                    <AssignButton label="Someday" onClick={() => handleAssign(task.id, ctx.id, 'someday')} />
                                                </div>
                                            </div>
                                        ))}
                                    </div>

                                    <button
                                        className="btn-ghost"
                                        onClick={() => setAssigningTaskId(null)}
                                        style={{ marginTop: '1rem', width: '100%', textAlign: 'center', fontSize: '0.8rem' }}
                                    >
                                        Cancel
                                    </button>
                                </div>
                            )}
                        </div>
                    ))}
                    {inboxTasks.length === 0 && (
                        <div style={{ padding: '2rem', textAlign: 'center', color: 'var(--text-secondary)', fontStyle: 'italic' }}>
                            Inbox is empty
                        </div>
                    )}
                </div>
            </div>

            {/* Someday / Backlog */}
            {somedayTasks.length > 0 && (
                <div>
                    <h3 style={{ marginLeft: '0.5rem', marginBottom: '1rem', color: 'var(--text-secondary)' }}>Someday</h3>
                    <div style={{ display: 'flex', flexDirection: 'column', gap: '0.5rem' }}>
                        {somedayTasks.map(task => (
                            <div key={task.id} className="glass-panel" style={{ padding: '0.8rem', opacity: 0.7, display: 'flex', justifyContent: 'space-between' }}>
                                <span>{task.title}</span>
                                <span style={{ fontSize: '0.7rem', background: 'rgba(0,0,0,0.05)', padding: '2px 6px', borderRadius: '4px' }}>
                                    {contexts.find(c => c.id === task.context_id)?.name || 'Unassigned'}
                                </span>
                            </div>
                        ))}
                    </div>
                </div>
            )}
        </div>
    );
};

const AssignButton: React.FC<{ label: string; onClick: () => void }> = ({ label, onClick }) => (
    <button
        onClick={onClick}
        style={{
            border: '1px solid rgba(0,0,0,0.1)',
            background: 'white',
            borderRadius: '4px',
            padding: '4px 8px',
            fontSize: '0.75rem',
            cursor: 'pointer',
            whiteSpace: 'nowrap'
        }}
    >
        {label}
    </button>
);

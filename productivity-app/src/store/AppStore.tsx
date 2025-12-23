import React, { createContext, useContext, useEffect, useState } from 'react';
import type { AppState, Context, Task, TimeBucket } from '../types';

interface AppContextType extends AppState {
    setContext: (id: string) => void;
    addTask: (task: Omit<Task, 'id' | 'created_at' | 'updated_at'>) => void;
    updateTask: (id: string, updates: Partial<Task>) => void;
    moveTask: (id: string, newWhen: TimeBucket, newContextId?: string) => void;
    deleteTask: (id: string) => void;
    canAddToday: (contextId: string) => boolean;
    canAddWeekPriority: (contextId: string) => boolean;
}

const AppContext = createContext<AppContextType | undefined>(undefined);

const INITIAL_CONTEXTS: Context[] = [
    { id: 'work', name: 'Work', sort_order: 0, theme: 'work' },
    { id: 'home', name: 'Home', sort_order: 1, theme: 'home' },
    { id: 'personal', name: 'Personal', sort_order: 2, theme: 'personal' },
];

const STORAGE_KEY = 'productivity-app-v1';

export const AppProvider: React.FC<{ children: React.ReactNode }> = ({ children }) => {
    const [state, setState] = useState<AppState>(() => {
        const stored = localStorage.getItem(STORAGE_KEY);
        if (stored) {
            try {
                const parsed = JSON.parse(stored);
                // Validate shape to prevent crashes
                if (Array.isArray(parsed.tasks) && Array.isArray(parsed.contexts)) {
                    return parsed;
                }
                console.warn('Stored state has invalid shape, resetting.');
            } catch (e) {
                console.error('Failed to parse stored state, resetting:', e);
            }
        }
        return {
            contexts: INITIAL_CONTEXTS,
            tasks: [],
            activeContextId: 'work',
        };
    });

    useEffect(() => {
        localStorage.setItem(STORAGE_KEY, JSON.stringify(state));
    }, [state]);

    const setContext = (id: string) => {
        setState(prev => ({ ...prev, activeContextId: id }));
    };

    const addTask = (taskData: Omit<Task, 'id' | 'created_at' | 'updated_at'>) => {
        const newTask: Task = {
            ...taskData,
            id: crypto.randomUUID(),
            created_at: Date.now(),
            updated_at: Date.now(),
        };
        setState(prev => ({ ...prev, tasks: [...prev.tasks, newTask] }));
    };

    const updateTask = (id: string, updates: Partial<Task>) => {
        setState(prev => {
            const newTasks = prev.tasks.map(t => {
                if (t.id !== id) return t;

                // Logic: If setting status to 'doing', ensure no other task in this context is 'doing'
                if (updates.status === 'doing') {
                    // We handle this in a separate effect or here? 
                    // Let's handle it here for simplicity, but we need to know the context.
                    // The task might be changing context too, but usually not when starting.
                    // We'll do a second pass to unset others if needed.
                }
                return { ...t, ...updates, updated_at: Date.now() };
            });

            // Enforce Single "Now" per Context
            if (updates.status === 'doing') {
                const updatedTask = newTasks.find(t => t.id === id);
                if (updatedTask && updatedTask.context_id) {
                    return {
                        ...prev,
                        tasks: newTasks.map(t => {
                            if (t.id !== id && t.context_id === updatedTask.context_id && t.status === 'doing') {
                                return { ...t, status: 'todo', updated_at: Date.now() };
                            }
                            return t;
                        })
                    };
                }
            }

            return { ...prev, tasks: newTasks };
        });
    };

    const moveTask = (id: string, newWhen: TimeBucket, newContextId?: string) => {
        const updates: Partial<Task> = {
            when: newWhen,
            ...(newContextId ? { context_id: newContextId } : {})
        };

        // Sync is_week_priority with the time bucket
        if (newWhen === 'this_week') {
            updates.is_week_priority = true;
        } else {
            updates.is_week_priority = false;
        }

        updateTask(id, updates);
    };

    const deleteTask = (id: string) => {
        setState(prev => ({ ...prev, tasks: prev.tasks.filter(t => t.id !== id) }));
    };

    const canAddToday = (contextId: string) => {
        const count = state.tasks.filter(t =>
            t.context_id === contextId &&
            t.when === 'today' &&
            t.status !== 'done'
        ).length;
        return count < 3;
    };

    const canAddWeekPriority = (contextId: string) => {
        const count = state.tasks.filter(t =>
            t.context_id === contextId &&
            t.is_week_priority
        ).length;
        return count < 3;
    };

    return (
        <AppContext.Provider value={{
            ...state,
            setContext,
            addTask,
            updateTask,
            moveTask,
            deleteTask,
            canAddToday,
            canAddWeekPriority
        }}>
            {children}
        </AppContext.Provider>
    );
};

export const useAppStore = () => {
    const context = useContext(AppContext);
    if (!context) throw new Error('useAppStore must be used within AppProvider');
    return context;
};
